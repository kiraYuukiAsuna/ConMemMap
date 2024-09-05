module;
#include <iostream>
#include <Windows.h>
#include <psapi.h>

export module Util;

export namespace WRL {
    size_t GetCurrentMemoryUsage() {
        PROCESS_MEMORY_COUNTERS pmc;
        if (GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc))) {
            std::cout << "Current memory usage: " << pmc.WorkingSetSize / 1024 << " KB" << std::endl;
            return pmc.WorkingSetSize;
        }
        else {
            std::cerr << "Failed to get memory information." << std::endl;
        }
        return 0;
    }

}
