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
    Str() : m_str{""} {};
    Str(std::string s) : m_str{s} {};

    std::string getString() const { return m_str; }

    friend std::istream &operator>>(std::istream &is, Str &str);
};

std::istream &operator>>(std::istream &is, Str &str)
{
    std::string temp;

    while (getline(is, temp))
    {
        str.m_str = temp;
    }

    return is;
}

std::ostream &operator<<(std::ostream &os, const Str &str)
{
    os << str.getString();
    return os;
}

std::string operator-(const Str &str)
{
    return "(-" + str.getString() + ")";
}

std::string operator-(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "-" + str2.getString() + ")";
}

std::string operator+(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "+" + str2.getString() + ")";
}

std::string operator*(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "*" + str2.getString() + ")";
}