/*
 * Names: Michel Rummens, Thomas Vos
 * StudentIDs:13108093, 12829501
 * This program defines a custom string, on which the operations +,-,~ and *
 * can be used.
 */

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

/*! Represents a custom string. */
class Str
{
private:
    std::string m_str;

public:
    /* Constructors for no data, and for a given string. */
    Str() : m_str{""} {};
    Str(std::string s) : m_str{s} {};

    /* Returns the actual string. */
    std::string getString() const { return m_str; }

    friend std::istream &operator>>(std::istream &is, Str &str);
};

/*
 * Reads a Str from 'is' stream.
 *
 * @param is Input stream, with a string.
 * @param str An empty Str object.
 * @return the now empty input stream.
 */
std::istream &operator>>(std::istream &is, Str &str)
{
    std::string temp;

    while (getline(is, temp))
    {
        str.m_str = temp;
    }

    return is;
}

/*
 * Writes Str 'str' to 'os' stream.
 *
 * @param os Output stream, empty at first.
 * @param str A Str object, to be put into the output stream.
 * @return the output stream, with the string of the Str object.
 */
std::ostream &operator<<(std::ostream &os, const Str &str)
{
    os << str.getString();
    return os;
}

/*
 * Returns the negation of a Str object.
 *
 * @param str A Str object, to be negated.
 * @return the negated Str as a string.
 */
std::string operator-(const Str &str)
{
    return "(-" + str.getString() + ")";
}

/*
 * Returns the subtraction of two Str objects.
 *
 * @param str1/str2 Two Str objects, to be subtracted from each other.
 * @return the result of the subtraction, as a string.
 */
std::string operator-(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "-" + str2.getString() + ")";
}

/*
 * Returns the addition of two Str objects.
 *
 * @param str1/str2 Two Str objects, to be added.
 * @return the result of the addition, as a string.
 */
std::string operator+(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "+" + str2.getString() + ")";
}

/*
 * Returns the multiplication of two Str objects.
 *
 * @param str1/str2 Two Str objects, to be multiplied.
 * @return the result of the multiplication, as a string.
 */
std::string operator*(const Str &str1, const Str &str2)
{
    return "(" + str1.getString() + "*" + str2.getString() + ")";
}