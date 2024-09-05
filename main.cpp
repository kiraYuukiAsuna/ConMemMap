#include "nlohmann/json.hpp"
#include <hopscotch_map.h>
#include <iostream>
#include <fstream>
#include <random>
#include <map>
#include <unordered_map>
#include "FileIo/SwcIo.hpp"

import ConMemMap;
import Util;
import PerfUtil;

//int main() {
//	WRL::ConMemMap<std::string, float> price;
//	std::unordered_map<std::string, float> price2;
//	tsl::hopscotch_map<std::string, float> price3;
//
//
//	price["apple"] = 1.0f;
//	price["banana"] = 2.0f;
//	price["orange"] = 3.0f;
//
//	price2["apple"] = 1.0f;
//	price2["banana"] = 2.0f;
//	price2["orange"] = 3.0f;
//
//	price3["apple"] = 1.0f;
//	price3["banana"] = 2.0f;
//	price3["orange"] = 3.0f;
//
//	std::cout << price["apple"] << std::endl;
//	std::cout << price["banana"] << std::endl;
//	std::cout << price["orange"] << std::endl;
//
//	std::cout << price2["apple"] << std::endl;
//	std::cout << price2["banana"] << std::endl;
//	std::cout << price2["orange"] << std::endl;
//
//	std::cout << price3["apple"] << std::endl;
//	std::cout << price3["banana"] << std::endl;
//	std::cout << price3["orange"] << std::endl;
//
//	price.find("banana") != price.end() ? std::cout << "found" << std::endl : std::cout << "not found" << std::endl;
//	price2.find("banana") != price2.end() ? std::cout << "found" << std::endl : std::cout << "not found" << std::endl;
//	price3.find("banana") != price3.end() ? std::cout << "found" << std::endl : std::cout << "not found" << std::endl;
//
//	return 0;
//}
template<typename T>
void TestSuite(PerformanceTest& test, std::vector<float>& randomDataX, std::vector<float>& randomDataY, std::vector<float>& randomDataZ, std::vector<int>& randomIndex) {
	T map;
	if (test.testtype == "Insert") {
		auto memBefore = WRL::GetCurrentMemoryUsage();
		std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
		for (int i = 0; i < test.size; i++) {
			map[i] = i;
		}
		std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
		test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
		std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
			<< std::endl;
		std::cout << map.size() << std::endl;
		auto memAfter = WRL::GetCurrentMemoryUsage();
		std::cout << "memory: " << memAfter - memBefore << std::endl;
		test.memory = memAfter - memBefore;
	}
	else if (test.testtype == "Find") {
		for (int i = 0; i < test.size; i++) {
			map[i] = i;
		}
		std::vector<float> vals;
		vals.reserve(test.size);
		int count = 0;
		std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();

		for (int i = 0; i < test.size; i++) {
			auto iter = map.find(i);
			if (iter != map.end()) {
				count++;
				// vals.emplace_back(iter->second);
			}
		}

		//for (auto r : randomIndex) {
		//	auto iter = map.find(std::make_tuple(randomDataX[r], randomDataY[r], randomDataZ[r]));
		//	if (iter != map.end()) {
		//		vals.emplace_back(iter->second);
		//	}
		//}

		std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
		test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
		std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
			<< std::endl;
		std::cout << count << std::endl;
	}
	else if (test.testtype == "Erase") {
		for (int i = 0; i < test.size; i++) {
			map[i] = i;
		}
		std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
		for (int i = 0; i < test.size; i++) {
			auto iter = map.find(i);
			if (iter != map.end()) {
				map.erase(iter);
			}
		}
		std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
		test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
		std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
			<< std::endl;
		std::cout << map.size() << std::endl;
	}
}
struct TupleHash {
	std::size_t operator()(const std::tuple<float, float, float>& t) const {
		auto hash1 = std::hash<float>{}(std::get<0>(t));
		auto hash2 = std::hash<float>{}(std::get<1>(t));
		auto hash3 = std::hash<float>{}(std::get<2>(t));
		return hash1 ^ (hash2 << 1) ^ (hash3 << 2);
	}
};

int main() {
	PerformanceTestCollection tests;

	for (int start = 100000; start <= 1000000; start += 100000) {
		tests.tests.emplace_back(generateTest("ConMemMap", start, "Insert"));
		tests.tests.emplace_back(generateTest("ConMemMap", start, "Find"));
		tests.tests.emplace_back(generateTest("ConMemMap", start, "Erase"));
		tests.tests.emplace_back(generateTest("std_unordered_map", start, "Insert"));
		tests.tests.emplace_back(generateTest("std_unordered_map", start, "Find"));
		tests.tests.emplace_back(generateTest("std_unordered_map", start, "Erase"));
		tests.tests.emplace_back(generateTest("std_map", start, "Insert"));
		tests.tests.emplace_back(generateTest("std_map", start, "Find"));
		tests.tests.emplace_back(generateTest("std_map", start, "Erase"));
		tests.tests.emplace_back(generateTest("hopscotch_map", start, "Insert"));
		tests.tests.emplace_back(generateTest("hopscotch_map", start, "Find"));
		tests.tests.emplace_back(generateTest("hopscotch_map", start, "Erase"));
	}

	std::random_device randomDevice;
	std::mt19937 gen(randomDevice());
	for (auto& test : tests.tests) {
		std::uniform_real_distribution<float> distribution(1.0f, (float)test.size);
		std::uniform_int<int> distributionIndex(0, test.size-1);

		std::vector<float> randomDataX;
		randomDataX.reserve(test.size);
		std::vector<float> randomDataY;
		randomDataY.reserve(test.size);
		std::vector<float> randomDataZ;
		randomDataZ.reserve(test.size);

		std::vector<int> randomIndex;
		randomIndex.reserve(test.size);

		for (int i = 0; i < test.size; ++i) {
			randomDataX.push_back(distribution(gen));
		}
		for (int i = 0; i < test.size; ++i) {
			randomDataY.push_back(distribution(gen));
		}
		for (int i = 0; i < test.size; ++i) {
			randomDataZ.push_back(distribution(gen));
		}

		for (int i = 0; i < test.size; ++i) {
			randomIndex.push_back(distributionIndex(gen));
		}

		if (test.maptype == "ConMemMap") {
			TestSuite<WRL::ConMemMap<float, float>>(test, randomDataX, randomDataY, randomDataZ, randomIndex);
		}
		else if (test.maptype == "std_unordered_map") {
			TestSuite<std::unordered_map<float, float>>(test, randomDataX, randomDataY, randomDataZ, randomIndex);
		}
		else if (test.maptype == "std_map") {
			TestSuite<std::map<float, float>>(test, randomDataX, randomDataY, randomDataZ, randomIndex);
		}
		else if (test.maptype == "hopscotch_map") {
			TestSuite<tsl::hopscotch_map<float, float>>(test, randomDataX, randomDataY, randomDataZ, randomIndex);
		}
	}

	nlohmann::json j;
	std::ofstream ofstream("TestResultLow.json");
	j = tests;
	ofstream << j.dump(4);
	ofstream.close();

	return 0;
}
