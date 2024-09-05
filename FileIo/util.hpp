#pragma once

#include <sstream>
#include <string>
#include <vector>
#include <ctime>
#include <iomanip>

struct RGB {
    int r, g, b;
};

// Helper function to convert HSL to RGB
inline RGB hslToRgb(float h, float s, float l) {
    // Ensure h is in [0, 1]
    h = fmod(h, 1.0f);
    float r, g, b;

    auto hueToRgb = [](float p, float q, float t) {
        if (t < 0.0f) t += 1.0f;
        if (t > 1.0f) t -= 1.0f;
        if (t < 1.0f / 6.0f) return p + (q - p) * 6.0f * t;
        if (t < 1.0f / 2.0f) return q;
        if (t < 2.0f / 3.0f) return p + (q - p) * (2.0f / 3.0f - t) * 6.0f;
        return p;
    };

    if (s == 0.0f) {
        r = g = b = l; // achromatic
    }
    else {
        float q = l < 0.5f ? l * (1.0f + s) : l + s - l * s;
        float p = 2.0f * l - q;
        r = hueToRgb(p, q, h + 1.0f / 3.0f);
        g = hueToRgb(p, q, h);
        b = hueToRgb(p, q, h - 1.0f / 3.0f);
    }
    return {int(r * 255), int(g * 255), int(b * 255)};
}

// Function to generate n distinct colors
inline std::vector<RGB> generateColors(int n) {
    std::vector<RGB> colors;
    float golden_ratio_conjugate = 0.618033988749895f;
    float h = 0.0f; // Initial hue

    for (int i = 0; i < n; ++i) {
        colors.push_back(hslToRgb(h, 0.5f, 0.6f)); // You can adjust saturation and lightness
        h += golden_ratio_conjugate;
        h = fmod(h, 1.0f);
    }

    return colors;
}


inline std::vector<std::string> stringSplit(const std::string&str, char delim) {
    std::stringstream ss(str);
    std::string item;
    std::vector<std::string> elems;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

inline std::string subreplace(std::string resource_str, std::string sub_str, std::string new_str) {
    std::string::size_type pos = 0;
    while ((pos = resource_str.find(sub_str)) != std::string::npos) //替换所有指定子串
    {
        resource_str.replace(pos, sub_str.length(), new_str);
    }
    return resource_str;
}

// Function to convert uint64 timestamp to date string
inline std::string timestampToString(uint64_t timestamp) {
    std::time_t timeT = timestamp;
    std::tm* timeStruct = std::localtime(&timeT);

    char buffer[128];
    std::strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", timeStruct);

    return buffer;
}

// Function to convert date string to uint64 timestamp
inline uint64_t stringToTimestamp(const std::string&time_str) {
    std::tm tm = {};
    std::stringstream ss(time_str);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    std::time_t time = std::mktime(&tm);
    if (time == -1) {
        return 0;
    }
    return static_cast<uint64_t>(time);
}
