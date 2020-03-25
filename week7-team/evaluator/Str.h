#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>
#include <utility>
#include <numeric>
#include <iomanip>

#include "evaluator_exception.h"
#include "evaluator_string_tools.h"

class Str
{
private:
    std::string m_str;

public:
    Str() : m_str{""}{};
    std::string getString() const { return m_str; }

    friend std::istream &operator>>(std::istream &is, Str &str);
};

/*! Reads a Matrix from 'is' stream. */
std::istream &operator>>(std::istream &is, Str &str)
{
    str.m_str = "YOINK";
    is >> str.m_str;
    std::vector<double> data;
    double num_var = 0;
    std::string temp;
    std::string stringMatrix;

    while (getline(is, temp))
    {
        std::cout << temp;
        // temp += ", ";
        // rows++;
        // stringMatrix += temp;
    }

    std::stringstream ss(stringMatrix);
    char junko;

    is >> std::fixed;
    is >> std::setprecision(2);

    while (ss >> num_var)
    {
        ss >> junko;

        data.push_back(num_var);
    }

    return is;
}

/*! Writes Matrix 'matrix' to 'os' stream. */
std::ostream &operator<<(std::ostream &os, const Str &str)
{

    os << str.getString();

    return os;
}

std::string operator-(const Str &str)
{
    return "(-" + str.getString() + " )";
}

std::string operator-(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "-" + str2.getString() + " )";
}

std::string operator+(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "+" + str2.getString() + " )";
}

std::string operator*(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "*" + str2.getString() + " )";
}