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
public:

    Str()
    {

    }

    //std::string getString() {  }

private:
    std::string m_str;
};

/*! Reads a Matrix from 'is' stream. */
std::istream &operator>>(std::istream &is, Str &str)
{
    std::vector<double> data;
    double num_var = 0;
    int rows = 0;
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
