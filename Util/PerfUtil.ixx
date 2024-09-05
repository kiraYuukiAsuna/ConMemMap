module;

#include "nlohmann/json.hpp"

export module PerfUtil;

export
{
    struct PerformanceTest {
        std::string name;
        std::string maptype;
        std::string testtype;
        int size;
        int memory;
        double time;

        friend void to_json(nlohmann::json&j, const PerformanceTest&test) {
            j["name"] = test.name;
            j["maptype"] = test.maptype;
            j["testtype"] = test.testtype;
            j["size"] = test.size;
            j["memory"] = test.memory;
            j["time"] = test.time;
        }

        friend void from_json(const nlohmann::json&j, PerformanceTest&test) {
            test.name = j.value("name", test.name);
            test.maptype = j.value("maptype", test.maptype);
            test.testtype = j.value("testtype", test.testtype);
            test.size = j.value("size", test.size);
            test.memory = j.value("memory", test.memory);
            test.time = j.value("time", test.time);
        }
    };

    struct PerformanceTestCollection {
        std::vector<PerformanceTest> tests;

        friend void to_json(nlohmann::json&j, const PerformanceTestCollection&test) {
            j["tests"] = test.tests;
        }

        friend void from_json(const nlohmann::json&j, PerformanceTestCollection&test) {
            test.tests = j.value("tests", test.tests);
        }
    };

    PerformanceTest generateTest(std::string maptype, int current, std::string testtype) {
        PerformanceTest test;
        test.maptype = maptype;
        test.testtype = testtype;
        test.size = current;
        test.memory = 0;
        test.time = 0;
        test.name = maptype + std::to_string(current) + "_" + test.testtype;
        return test;
    }
}
