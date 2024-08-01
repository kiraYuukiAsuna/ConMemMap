module;
#include <cassert>
#include <memory>
#include <vector>
#include <cmath>

export module ConMemMap;

export namespace WRL {
    template<
        typename Key,
        typename Type,
        typename = std::hash<Key>,
        typename = std::equal_to<Key>,
        typename = std::allocator<std::pair<const Key, Type>>>
    class ConMemMap;

    template<typename Type, typename = void>
    struct is_transparent : std::false_type {
    };

    template<typename Type>
    struct is_transparent<Type, std::void_t<typename Type::is_transparent>> : std::true_type {
    };

    template<typename Type>
    inline constexpr bool is_transparent_v = is_transparent<Type>::value;

    template<typename Type>
    struct uses_allocator_construction {
        template<typename Allocator, typename... Params>
        static constexpr auto args([[maybe_unused]] const Allocator&allocator, Params&&... params) noexcept {
            if constexpr (!std::uses_allocator_v<Type, Allocator> && std::is_constructible_v<Type, Params...>) {
                return std::forward_as_tuple(std::forward<Params>(params)...);
            }
            else {
                static_assert(std::uses_allocator_v<Type, Allocator>, "Ill-formed request");

                if constexpr (std::is_constructible_v<Type, std::allocator_arg_t, const Allocator &, Params...>) {
                    return std::tuple<std::allocator_arg_t, const Allocator &, Params &&...>{
                        std::allocator_arg, allocator, std::forward<Params>(params)...
                    };
                }
                else {
                    static_assert(std::is_constructible_v<Type, Params..., const Allocator &>,
                                  "Ill-formed request");
                    return std::forward_as_tuple(std::forward<Params>(params)..., allocator);
                }
            }
        }
    };

    template<typename Type, typename Other>
    struct uses_allocator_construction<std::pair<Type, Other>> {
        using type = std::pair<Type, Other>;

        template<typename Allocator, typename First, typename Second>
        static constexpr auto args(const Allocator&allocator, std::piecewise_construct_t, First&&first,
                                   Second&&second) noexcept {
            return std::make_tuple(
                std::piecewise_construct,
                std::apply([&allocator](auto&&... curr) {
                    return uses_allocator_construction<
                        type>::args(allocator, std::forward<decltype(curr)>(curr)...);
                }, std::forward<First>(first)),
                std::apply([&allocator](auto&&... curr) {
                    return uses_allocator_construction<Other>::args(
                        allocator, std::forward<decltype(curr)>(curr)...);
                }, std::forward<Second>(second)));
        }

        template<typename Allocator>
        static constexpr auto args(const Allocator&allocator) noexcept {
            return uses_allocator_construction<type>::args(allocator, std::piecewise_construct, std::tuple<>{},
                                                           std::tuple<>{});
        }

        template<typename Allocator, typename First, typename Second>
        static constexpr auto args(const Allocator&allocator, First&&first, Second&&second) noexcept {
            return uses_allocator_construction<type>::args(allocator, std::piecewise_construct,
                                                           std::forward_as_tuple(std::forward<First>(first)),
                                                           std::forward_as_tuple(std::forward<Second>(second)));
        }

        template<typename Allocator, typename First, typename Second>
        static constexpr auto args(const Allocator&allocator, const std::pair<First, Second>&value) noexcept {
            return uses_allocator_construction<type>::args(allocator, std::piecewise_construct,
                                                           std::forward_as_tuple(value.first),
                                                           std::forward_as_tuple(value.second));
        }

        template<typename Allocator, typename First, typename Second>
        static constexpr auto args(const Allocator&allocator, std::pair<First, Second>&&value) noexcept {
            return uses_allocator_construction<type>::args(allocator, std::piecewise_construct,
                                                           std::forward_as_tuple(std::move(value.first)),
                                                           std::forward_as_tuple(std::move(value.second)));
        }
    };

    template<typename Key, typename Type>
    class ElementEntity {
        using ValueType = std::pair<Key, Type>;

    public:
        std::size_t Next;
        ValueType Element;

        template<typename... Args>
        ElementEntity(const std::size_t pos, Args&&... args)
            : Next{pos},
              Element{std::forward<Args>(args)...} {
        }

        template<typename Allocator, typename... Args>
        ElementEntity(std::allocator_arg_t, const Allocator&allocator, const std::size_t pos, Args&&... args)
            : Next{pos},
              Element{
                  std::make_from_tuple<ValueType>(
                      uses_allocator_construction<ValueType>::args(allocator, std::forward<Args>(args)...))
              } {
        }

        template<typename Allocator>
        ElementEntity(std::allocator_arg_t, const Allocator&allocator, const ElementEntity&other)
            : Next{other.Next},
              Element{
                  std::make_from_tuple<ValueType>(
                      uses_allocator_construction<ValueType>::args(allocator, other.Element))
              } {
        }

        template<typename Allocator>
        ElementEntity(std::allocator_arg_t, const Allocator&allocator, ElementEntity&&other)
            : Next{other.Next},
              Element{
                  std::make_from_tuple<ValueType>(
                      uses_allocator_construction<ValueType>::args(allocator, std::move(other.Element)))
              } {
        }
    };

    template<typename Type>
    struct InputIteratorPointer final {
        using ValueType = Type;
        using Pointer = Type *;
        using Reference = Type &;

        constexpr InputIteratorPointer(ValueType&&value) noexcept(std::is_nothrow_move_constructible_v<ValueType>)
            : Value{std::move(value)} {
        }

        [[nodiscard]] constexpr Pointer operator->() noexcept {
            return std::addressof(Value);
        }

        [[nodiscard]] constexpr Reference operator*() noexcept {
            return Value;
        }

    private:
        Type Value;
    };

    template<typename Iter>
    class ElementIterator final {
        template<typename Iter>
        friend class ElementIterator;

        using FirstType = decltype(std::as_const(std::declval<Iter>()->Element.first));
        using SecondType = decltype((std::declval<Iter>()->Element.second));

    public:
        using ValueType = std::pair<FirstType, SecondType>;
        using Pointer = InputIteratorPointer<ValueType>;
        using Reference = ValueType;
        using DifferenceType = std::ptrdiff_t;
        using IteratorCategory = std::input_iterator_tag;

        constexpr ElementIterator() noexcept
            : m_Iter{} {
        }

        constexpr ElementIterator(const Iter iter) noexcept
            : m_Iter{iter} {
        }

        template<typename Other, typename = std::enable_if_t<
            !std::is_same_v<Iter, Other> && std::is_constructible_v<Iter, Other>>>
        constexpr ElementIterator(const ElementIterator<Other>&other) noexcept
            : m_Iter{other.m_Iter} {
        }

        constexpr ElementIterator& operator++() noexcept {
            return ++m_Iter, *this;
        }

        constexpr ElementIterator operator++(int) noexcept {
            ElementIterator orig = *this;
            return ++(*this), orig;
        }

        constexpr ElementIterator& operator--() noexcept {
            return --m_Iter, *this;
        }

        constexpr ElementIterator operator--(int) noexcept {
            ElementIterator orig = *this;
            return operator--(), orig;
        }

        constexpr ElementIterator& operator+=(const DifferenceType value) noexcept {
            m_Iter += value;
            return *this;
        }

        constexpr ElementIterator operator+(const DifferenceType value) const noexcept {
            ElementIterator copy = *this;
            return (copy += value);
        }

        constexpr ElementIterator& operator-=(const DifferenceType value) noexcept {
            return (*this += -value);
        }

        constexpr ElementIterator operator-(const DifferenceType value) const noexcept {
            return (*this + -value);
        }

        [[nodiscard]] constexpr Reference operator[](const DifferenceType value) const noexcept {
            return {m_Iter[value].element.first, m_Iter[value].element.second};
        }

        [[nodiscard]] constexpr Pointer operator->() const noexcept {
            return operator*();
        }

        [[nodiscard]] constexpr Reference operator*() const noexcept {
            return {m_Iter->Element.first, m_Iter->Element.second};
        }

        template<typename ILhs, typename IRhs>
        friend constexpr std::ptrdiff_t operator-(const ElementIterator<ILhs>&,
                                                  const ElementIterator<IRhs>&) noexcept;

        template<typename ILhs, typename IRhs>
        friend constexpr bool operator==(const ElementIterator<ILhs>&, const ElementIterator<IRhs>&) noexcept;

        template<typename ILhs, typename IRhs>
        friend constexpr bool operator<(const ElementIterator<ILhs>&, const ElementIterator<IRhs>&) noexcept;

    private:
        Iter m_Iter;
    };

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr std::ptrdiff_t operator-(const ElementIterator<ILhs>&lhs,
                                                     const ElementIterator<IRhs>&rhs) noexcept {
        return lhs.m_Iter - rhs.m_Iter;
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator==(const ElementIterator<ILhs>&lhs,
                                            const ElementIterator<IRhs>&rhs) noexcept {
        return lhs.m_Iter == rhs.m_Iter;
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator!=(const ElementIterator<ILhs>&lhs,
                                            const ElementIterator<IRhs>&rhs) noexcept {
        return !(lhs == rhs);
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator<(const ElementIterator<ILhs>&lhs,
                                           const ElementIterator<IRhs>&rhs) noexcept {
        return lhs.m_Iter < rhs.m_Iter;
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator>(const ElementIterator<ILhs>&lhs,
                                           const ElementIterator<IRhs>&rhs) noexcept {
        return rhs < lhs;
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator<=(const ElementIterator<ILhs>&lhs,
                                            const ElementIterator<IRhs>&rhs) noexcept {
        return !(lhs > rhs);
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator>=(const ElementIterator<ILhs>&lhs,
                                            const ElementIterator<IRhs>&rhs) noexcept {
        return !(lhs < rhs);
    }

    template<typename Iter>
    class ElementLocaliterator final {
        template<typename Iter>
        friend class ElementLocaliterator;

        using FirstType = decltype(std::as_const(std::declval<Iter>()->Element.first));
        using SecondType = decltype((std::declval<Iter>()->Element.second));

    public:
        using ValueType = std::pair<FirstType, SecondType>;
        using Pointer = InputIteratorPointer<ValueType>;
        using Reference = ValueType;
        using DifferenceType = std::ptrdiff_t;
        using IteratorCategory = std::input_iterator_tag;

        constexpr ElementLocaliterator() noexcept
            : m_Iter{},
              Offset{} {
        }

        constexpr ElementLocaliterator(Iter iter, const std::size_t pos) noexcept
            : m_Iter{iter},
              Offset{pos} {
        }

        template<typename Other, typename = std::enable_if_t<
            !std::is_same_v<Iter, Other> && std::is_constructible_v<Iter, Other>>>
        constexpr ElementLocaliterator(const ElementLocaliterator<Other>&other) noexcept
            : m_Iter{other.it},
              Offset{other.offset} {
        }

        constexpr ElementLocaliterator& operator++() noexcept {
            return Offset = m_Iter[Offset].Next, *this;
        }

        constexpr ElementLocaliterator operator++(int) noexcept {
            ElementLocaliterator orig = *this;
            return ++(*this), orig;
        }

        [[nodiscard]] constexpr Pointer operator->() const noexcept {
            return operator*();
        }

        [[nodiscard]] constexpr Reference operator*() const noexcept {
            return {m_Iter[Offset].Element.first, m_Iter[Offset].Element.second};
        }

        [[nodiscard]] constexpr std::size_t index() const noexcept {
            return Offset;
        }

    private:
        Iter m_Iter;
        std::size_t Offset;
    };

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator==(const ElementLocaliterator<ILhs>&lhs,
                                            const ElementLocaliterator<IRhs>&rhs) noexcept {
        return lhs.index() == rhs.index();
    }

    template<typename ILhs, typename IRhs>
    [[nodiscard]] constexpr bool operator!=(const ElementLocaliterator<ILhs>&lhs,
                                            const ElementLocaliterator<IRhs>&rhs) noexcept {
        return !(lhs == rhs);
    }


    [[nodiscard]] constexpr bool IsPowerOfTwo(const std::size_t value) noexcept {
        return value && ((value & (value - 1)) == 0);
    }

    [[nodiscard]] constexpr std::size_t NextPowerOfTwo(const std::size_t value) noexcept {
        assert(value < (std::size_t{1u} << (std::numeric_limits<std::size_t>::digits - 1))); // "Numeric limits exceeded"
        std::size_t curr = value - (value != 0u);

        for (int next = 1; next < std::numeric_limits<std::size_t>::digits; next = next * 2) {
            curr |= curr >> next;
        }

        return ++curr;
    }

    [[nodiscard]] constexpr std::size_t FastMod(const std::size_t value, const std::size_t mod) noexcept {
        assert(IsPowerOfTwo(mod)); // "Value must be a power of two"
        return value & (mod - 1u);
    }

    template<typename Key, typename Type, typename Hash, typename KeyEqual, typename Allocator>
    class ConMemMap {
        static constexpr float DefaultThreshold = 0.875f;
        static constexpr std::size_t MinimumCapacity = 8u;
        using ElementType = ElementEntity<Key, Type>;
        using AllocTraits = std::allocator_traits<Allocator>;
        static_assert(std::is_same_v<typename AllocTraits::value_type, std::pair<const Key, Type>>,
                      "Invalid Value Type");

        using SparseContainerType = std::vector<std::size_t, typename AllocTraits::template rebind_alloc<
            std::size_t>>;
        using PackedContainerType = std::vector<ElementType, typename AllocTraits::template rebind_alloc<ElementType>>;

        template<typename Other>
        [[nodiscard]] std::size_t KeyToBucket(const Other&key) const noexcept {
            return FastMod(static_cast<SizeType>(Sparse.second(key)), bucket_count());
        }

        template<typename Other>
        [[nodiscard]] auto ConstrainedFind(const Other&key, std::size_t bucket) {
            for (auto it = begin(bucket), last = end(bucket); it != last; ++it) {
                if (Packed.second(it->first, key)) {
                    return begin() + static_cast<typename Iterator::DifferenceType>(it.index());
                }
            }

            return end();
        }

        template<typename Other, typename... Args>
        [[nodiscard]] auto InsertOrDoNothing(Other&&key, Args&&... args) {
            const auto index = KeyToBucket(key);

            if (auto it = ConstrainedFind(key, index); it != end()) {
                return std::make_pair(it, false);
            }

            Packed.first.emplace_back(Sparse.first[index], std::piecewise_construct,
                                        std::forward_as_tuple(std::forward<Other>(key)),
                                        std::forward_as_tuple(std::forward<Args>(args)...));
            Sparse.first[index] = Packed.first.size() - 1u;
            RehashIfRequired();

            return std::make_pair(--end(), true);
        }

        template<typename Other, typename Arg>
        [[nodiscard]] auto InsertOrOverwrite(Other&&key, Arg&&value) {
            const auto index = KeyToBucket(key);

            if (auto it = ConstrainedFind(key, index); it != end()) {
                it->second = std::forward<Arg>(value);
                return std::make_pair(it, false);
            }

            Packed.first().emplace_back(Sparse.first()[index], std::forward<Other>(key), std::forward<Arg>(value));
            Sparse.first()[index] = Packed.first().size() - 1u;
            RehashIfRequired();

            return std::make_pair(--end(), true);
        }

        void MoveAndPop(const std::size_t pos) {
            if (const auto last = size() - 1u; pos != last) {
                SizeType* curr = Sparse.first.data() + KeyToBucket(Packed.first.back().Element.first);
                Packed.first[pos] = std::move(Packed.first.back());
                for (; *curr != last; curr = &Packed.first[*curr].Next) {
                }
                *curr = pos;
            }

            Packed.first.pop_back();
        }

        void RehashIfRequired() {
            if (size() > (bucket_count() * max_load_factor())) {
                rehash(bucket_count() * 2u);
            }
        }

    public:
        using SizeType = std::size_t;
        using Iterator = ElementIterator<typename PackedContainerType::iterator>;
        using ConstIterator = ElementIterator<typename PackedContainerType::const_iterator>;
        using LocalIterator = ElementLocaliterator<typename PackedContainerType::iterator>;
        using ConstLocalIterator = ElementLocaliterator<typename PackedContainerType::const_iterator>;
        using ValueType = std::pair<const Key, Type>;

        ConMemMap()
            : ConMemMap{MinimumCapacity} {
        }

        explicit ConMemMap(const Allocator&allocator)
            : ConMemMap{MinimumCapacity, Hash{}, KeyEqual{}, allocator} {
        }

        ConMemMap(const SizeType cnt, const Allocator&allocator)
            : ConMemMap{cnt, Hash{}, KeyEqual{}, allocator} {
        }

        ConMemMap(const SizeType cnt, const Hash&hash, const Allocator&allocator)
            : ConMemMap{cnt, hash, KeyEqual{}, allocator} {
        }

        explicit ConMemMap(const SizeType cnt, const Hash&hash = Hash{}, const KeyEqual&equal = KeyEqual{},
                           const Allocator&allocator = Allocator{})
            : Sparse{allocator, hash},
              Packed{allocator, equal},
              Threshold{DefaultThreshold} {
            rehash(cnt);
        }

        ConMemMap(const ConMemMap&) = default;

        ConMemMap(const ConMemMap&other, const Allocator&allocator)
            : Sparse{
                  std::piecewise_construct, std::forward_as_tuple(other.Sparse.first(), allocator),
                  std::forward_as_tuple(other.Sparse.second())
              },
              Packed{
                  std::piecewise_construct, std::forward_as_tuple(other.Packed.first(), allocator),
                  std::forward_as_tuple(other.Packed.second())
              },
              Threshold{other.Packed} {
        }

        ConMemMap(
            ConMemMap&&) noexcept(std::is_nothrow_move_constructible_v<std::pair<SparseContainerType, Hash>>
                                  && std::is_nothrow_move_constructible_v<std::pair<PackedContainerType,
                                      KeyEqual>>) = default;

        ConMemMap(ConMemMap&&other, const Allocator&allocator)
            : Sparse{
                  std::piecewise_construct, std::forward_as_tuple(std::move(other.Sparse.first()), allocator),
                  std::forward_as_tuple(std::move(other.Sparse.second()))
              },
              Packed{
                  std::piecewise_construct, std::forward_as_tuple(std::move(other.Packed.first()), allocator),
                  std::forward_as_tuple(std::move(other.Packed.second()))
              },
              Threshold{other.Packed} {
        }

        ConMemMap& operator=(const ConMemMap&) = default;

        ConMemMap& operator=(
            ConMemMap&&) noexcept(std::is_nothrow_move_assignable_v<std::pair<SparseContainerType, Hash>> &&
                                  std::is_nothrow_move_assignable_v<std::pair<PackedContainerType, KeyEqual>>)
        = default;

        [[nodiscard]] constexpr Allocator get_allocator() const noexcept {
            return Sparse.first.get_allocator();
        }

        [[nodiscard]] ConstIterator cbegin() const noexcept {
            return Packed.first.begin();
        }

        [[nodiscard]] ConstIterator begin() const noexcept {
            return cbegin();
        }

        [[nodiscard]] Iterator begin() noexcept {
            return Packed.first.begin();
        }

        [[nodiscard]] ConstIterator cend() const noexcept {
            return Packed.first.end();
        }

        [[nodiscard]] ConstIterator end() const noexcept {
            return cend();
        }

        [[nodiscard]] Iterator end() noexcept {
            return Packed.first.end();
        }

        [[nodiscard]] bool empty() const noexcept {
            return Packed.first.empty();
        }

        [[nodiscard]] SizeType size() const noexcept {
            return Packed.first.size();
        }

        [[nodiscard]] SizeType max_size() const noexcept {
            return Packed.first.max_size();
        }

        void clear() noexcept {
            Sparse.first.clear();
            Packed.first.clear();
            rehash(0u);
        }


        std::pair<Iterator, bool> insert(const ValueType&value) {
            return InsertOrDoNothing(value.first, value.second);
        }

        std::pair<Iterator, bool> insert(ValueType&&value) {
            return InsertOrDoNothing(std::move(value.first), std::move(value.second));
        }

        template<typename Arg>
        std::enable_if_t<std::is_constructible_v<ValueType, Arg &&>, std::pair<Iterator, bool>>
        insert(Arg&&value) {
            return InsertOrDoNothing(std::forward<Arg>(value).first, std::forward<Arg>(value).second);
        }

        template<typename It>
        void insert(It first, It last) {
            for (; first != last; ++first) {
                insert(*first);
            }
        }

        template<typename Arg>
        std::pair<Iterator, bool> insert_or_assign(const Key&key, Arg&&value) {
            return insert_or_overwrite(key, std::forward<Arg>(value));
        }

        template<typename Arg>
        std::pair<Iterator, bool> insert_or_assign(Key&&key, Arg&&value) {
            return insert_or_overwrite(std::move(key), std::forward<Arg>(value));
        }

        template<typename... Args>
        std::pair<Iterator, bool> emplace([[maybe_unused]] Args&&... args) {
            if constexpr (sizeof...(Args) == 0u) {
                return InsertOrDoNothing(Key{});
            }
            else if constexpr (sizeof...(Args) == 1u) {
                return InsertOrDoNothing(std::forward<Args>(args).first..., std::forward<Args>(args).second...);
            }
            else if constexpr (sizeof...(Args) == 2u) {
                return InsertOrDoNothing(std::forward<Args>(args)...);
            }
            else {
                auto&node = Packed.first().emplace_back(Packed.first().size(), std::forward<Args>(args)...);
                const auto index = KeyToBucket(node.element.first);

                if (auto it = ConstrainedFind(node.element.first, index); it != end()) {
                    Packed.first().pop_back();
                    return std::make_pair(it, false);
                }

                std::swap(node.next, Sparse.first()[index]);
                RehashIfRequired();

                return std::make_pair(--end(), true);
            }
        }

        template<typename... Args>
        std::pair<Iterator, bool> try_emplace(const Key&key, Args&&... args) {
            return InsertOrDoNothing(key, std::forward<Args>(args)...);
        }

        template<typename... Args>
        std::pair<Iterator, bool> try_emplace(Key&&key, Args&&... args) {
            return InsertOrDoNothing(std::move(key), std::forward<Args>(args)...);
        }

        Iterator erase(ConstIterator pos) {
            const auto diff = pos - cbegin();
            erase(pos->first);
            return begin() + diff;
        }

        Iterator erase(ConstIterator first, ConstIterator last) {
            const auto dist = first - cbegin();

            for (auto from = last - cbegin(); from != dist; --from) {
                erase(Packed.first()[from - 1u].element.first);
            }

            return (begin() + dist);
        }

        SizeType erase(const Key&key) {
            for (SizeType* curr = Sparse.first.data() + KeyToBucket(key);
                 *curr != (std::numeric_limits<SizeType>::max)(); curr = &Packed.first[*curr].Next) {
                if (Packed.second(Packed.first[*curr].Element.first, key)) {
                    const auto index = *curr;
                    *curr = Packed.first[*curr].Next;
                    MoveAndPop(index);
                    return 1u;
                }
            }

            return 0u;
        }

        void swap(ConMemMap&other) {
            std::swap(Sparse, other.Sparse);
            std::swap(Packed, other.Packed);
            std::swap(Threshold, other.Threshold);
        }

        [[nodiscard]] Type& at(const Key&key) {
            auto it = find(key);
            assert(it != end()); // "Invalid key"
            return it->second;
        }

        [[nodiscard]] const Type& at(const Key&key) const {
            auto it = find(key);
            assert(it != cend()); // "Invalid key"
            return it->second;
        }

        [[nodiscard]] Type& operator[](const Key&key) {
            return InsertOrDoNothing(key).first->second;
        }

        [[nodiscard]] Type& operator[](Key&&key) {
            return InsertOrDoNothing(std::move(key)).first->second;
        }

        [[nodiscard]] SizeType count(const Key&key) const {
            return find(key) != end();
        }

        template<typename Other>
        [[nodiscard]] std::enable_if_t<is_transparent_v<Hash> && is_transparent_v<KeyEqual>, std::conditional_t<false
            , Other, SizeType>>
        count(const Other&key) const {
            return find(key) != end();
        }

        [[nodiscard]] Iterator find(const Key&key) {
            return ConstrainedFind(key, KeyToBucket(key));
        }

        [[nodiscard]] ConstIterator find(const Key&key) const {
            return ConstrainedFind(key, KeyToBucket(key));
        }

        template<typename Other>
        [[nodiscard]] std::enable_if_t<is_transparent_v<Hash> && is_transparent_v<KeyEqual>, std::conditional_t<false
            , Other, Iterator>>
        find(const Other&key) {
            return ConstrainedFind(key, KeyToBucket(key));
        }

        template<typename Other>
        [[nodiscard]] std::enable_if_t<is_transparent_v<Hash> && is_transparent_v<KeyEqual>, std::conditional_t<false
            , Other, ConstIterator>>
        find(const Other&key) const {
            return ConstrainedFind(key, KeyToBucket(key));
        }

        [[nodiscard]] std::pair<Iterator, Iterator> equal_range(const Key&key) {
            const auto it = find(key);
            return {it, it + !(it == end())};
        }

        [[nodiscard]] std::pair<ConstIterator, ConstIterator> equal_range(const Key&key) const {
            const auto it = find(key);
            return {it, it + !(it == cend())};
        }

        template<typename Other>
        [[nodiscard]] std::enable_if_t<is_transparent_v<Hash> && is_transparent_v<KeyEqual>, std::conditional_t<false
            , Other, std::pair<Iterator, Iterator>>>
        equal_range(const Other&key) {
            const auto it = find(key);
            return {it, it + !(it == end())};
        }

        template<class Other>
        [[nodiscard]] std::enable_if_t<is_transparent_v<Hash> && is_transparent_v<KeyEqual>, std::conditional_t<false
            , Other, std::pair<ConstIterator, ConstIterator>>>
        equal_range(const Other&key) const {
            const auto it = find(key);
            return {it, it + !(it == cend())};
        }

        [[nodiscard]] bool contains(const Key&key) const {
            return find(key) != cend();
        }

        template<typename Other>
        [[nodiscard]] std::enable_if_t<is_transparent_v<Hash> && is_transparent_v<KeyEqual>, std::conditional_t<false
            , Other, bool>>
        contains(const Other&key) const {
            return find(key) != cend();
        }

        [[nodiscard]] ConstLocalIterator cbegin(const SizeType index) const {
            return {Packed.first().begin(), Sparse.first()[index]};
        }

        [[nodiscard]] ConstLocalIterator begin(const SizeType index) const {
            return cbegin(index);
        }

        [[nodiscard]] LocalIterator begin(const SizeType index) {
            return {Packed.first.begin(), Sparse.first[index]};
        }

        [[nodiscard]] ConstLocalIterator cend([[maybe_unused]] const SizeType index) const {
            return {Packed.first.begin(), (std::numeric_limits<SizeType>::max)()};
        }

        [[nodiscard]] ConstLocalIterator end(const SizeType index) const {
            return cend(index);
        }

        [[nodiscard]] LocalIterator end([[maybe_unused]] const SizeType index) {
            return {Packed.first.begin(), (std::numeric_limits<SizeType>::max)()};
        }

        [[nodiscard]] SizeType bucket_count() const {
            return Sparse.first.size();
        }

        [[nodiscard]] SizeType max_bucket_count() const {
            return Sparse.first.max_size();
        }

        [[nodiscard]] SizeType bucket_size(const SizeType index) const {
            return static_cast<SizeType>(std::distance(begin(index), end(index)));
        }

        [[nodiscard]] SizeType bucket(const Key&key) const {
            return KeyToBucket(key);
        }

        [[nodiscard]] float load_factor() const {
            return size() / static_cast<float>(bucket_count());
        }

        [[nodiscard]] float max_load_factor() const {
            return Threshold;
        }

        void max_load_factor(const float value) {
            assert(value > 0.f); // "Invalid load factor"
            Threshold = value;
            rehash(0u);
        }

        void rehash(const SizeType cnt) {
            auto value = cnt > MinimumCapacity ? cnt : MinimumCapacity;
            const auto cap = static_cast<SizeType>(size() / max_load_factor());
            value = value > cap ? value : cap;

            if (const auto sz = NextPowerOfTwo(value); sz != bucket_count()) {
                Sparse.first.resize(sz);

                for (auto&&elem: Sparse.first) {
                    elem = std::numeric_limits<SizeType>::max();
                }

                for (SizeType pos{}, last = size(); pos < last; ++pos) {
                    const auto index = KeyToBucket(Packed.first[pos].Element.first);
                    Packed.first[pos].Next = std::exchange(Sparse.first[index], pos);
                }
            }
        }

        void reserve(const SizeType cnt) {
            Packed.first().reserve(cnt);
            rehash(std::ceil(cnt / max_load_factor()));
        }

        [[nodiscard]] Hash hash_function() const {
            return Sparse.second();
        }

        [[nodiscard]] KeyEqual key_eq() const {
            return Packed.second();
        }

    private:
        std::pair<PackedContainerType, KeyEqual> Packed;
        std::pair<SparseContainerType, Hash> Sparse;
        float Threshold;
    };
}

namespace std {
    template<typename Key, typename Value, typename Allocator>
    struct uses_allocator<WRL::ElementEntity<Key, Value>, Allocator>
            : std::true_type {
    };
}
