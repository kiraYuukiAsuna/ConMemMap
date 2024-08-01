#include "nlohmann/json.hpp"

import std;
import ConMemMap;
import Util;
import PerfUtil;

int main() {
    WRL::ConMemMap<std::string, float> price;
    std::unordered_map<std::string, float> price2;

    price["apple"] = 1.0f;
    price["banana"] = 2.0f;
    price["orange"] = 3.0f;

    price2["apple"] = 1.0f;
    price2["banana"] = 2.0f;
    price2["orange"] = 3.0f;

    std::cout << price["apple"] << std::endl;
    std::cout << price["banana"] << std::endl;
    std::cout << price["orange"] << std::endl;

    std::cout << price2["apple"] << std::endl;
    std::cout << price2["banana"] << std::endl;
    std::cout << price2["orange"] << std::endl;

    price.find("banana") != price.end() ? std::cout << "found" << std::endl : std::cout << "not found" << std::endl;
    price2.find("banana") != price2.end() ? std::cout << "found" << std::endl : std::cout << "not found" << std::endl;

    return 0;
}

int smain() {
    PerformanceTestCollection tests;

    for (int start = 10000; start <= 1000000; start += 50000) {
        tests.tests.emplace_back(generateTestConMemMap("ConMemMap", start, "Insert"));
        tests.tests.emplace_back(generateTestConMemMap("ConMemMap", start, "Find"));
        tests.tests.emplace_back(generateTestConMemMap("ConMemMap", start, "Erase"));
        tests.tests.emplace_back(generateTestConMemMap("std_unordered_map", start, "Insert"));
        tests.tests.emplace_back(generateTestConMemMap("std_unordered_map", start, "Find"));
        tests.tests.emplace_back(generateTestConMemMap("std_unordered_map", start, "Erase"));
    }

    std::random_device randomDevice;
    std::mt19937 gen(randomDevice());
    for (auto&test: tests.tests) {
        std::uniform_real_distribution<float> distrib(1.0f, (float)test.size);
        std::vector<float> randomData;
        randomData.reserve(test.size);
        for (int i = 0; i < test.size; ++i) {
            randomData.push_back(distrib(gen));
        }
        if (test.maptype == "ConMemMap") {
            WRL::ConMemMap<float, float> map;
            if (test.testtype == "Insert") {
                std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
                for (int i = 0; i < test.size; i++) {
                    map[i] = randomData[i];
                }
                std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
                test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
                std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
                        << std::endl;
                std::cout << map.size() << std::endl;
                auto mem = WRL::GetCurrentMemoryUsage();
                std::cout << "memory: " << mem;
                test.memory = mem;
            }
            else if (test.testtype == "Find") {
                for (int i = 0; i < test.size; i++) {
                    map[i] = test.size;
                }
                std::vector<float> vals;
                vals.reserve(test.size);
                std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
                for (int i = 0; i < test.size; i++) {
                    auto iter = map.find(i);
                    if (iter != map.end()) {
                        vals.emplace_back(iter->second);
                    }
                }
                std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
                test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
                std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
                        << std::endl;
                std::cout << vals.size() << std::endl;
            }
            else if (test.testtype == "Erase") {
                for (int i = 0; i < test.size; i++) {
                    map[i] = randomData[i];
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
        else if (test.maptype == "std_unordered_map") {
            std::unordered_map<float, float> map;
            if (test.testtype == "Insert") {
                std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
                for (int i = 0; i < test.size; i++) {
                    map[i] = test.size;
                }
                std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
                test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
                std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
                        << std::endl;
                std::cout << map.size() << std::endl;
                auto mem = WRL::GetCurrentMemoryUsage();
                std::cout << "memory: " << mem;
                test.memory = mem;
            }
            else if (test.testtype == "Find") {
                for (int i = 0; i < test.size; i++) {
                    map[i] = test.size;
                }
                std::vector<float> vals;
                vals.reserve(test.size);
                std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
                for (int i = 0; i < test.size; i++) {
                    auto iter = map.find(i);
                    if (iter != map.end()) {
                        vals.emplace_back(iter->second);
                    }
                }
                std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
                test.time = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
                std::cout << "time: " << std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
                        << std::endl;
                std::cout << vals.size() << std::endl;
            }
            else if (test.testtype == "Erase") {
                for (int i = 0; i < test.size; i++) {
                    map[i] = test.size;
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
    }

    nlohmann::json j;
    std::ofstream ofstream("TestResultLow.json");
    j = tests;
    ofstream << j.dump(4);
    ofstream.close();
}
