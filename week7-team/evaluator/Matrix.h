#ifndef MATRIX_INCLUDED
#define MATRIX_INCLUDED

#include <vector>
#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>
#include <utility>
#include <numeric>

#include "evaluator_exception.h"
#include "evaluator_string_tools.h"

/*! Represents a 2 dimensional matrix of doubles.*/
class Matrix
{
    int m_rows, m_cols;
    std::vector<double> m_data;

public:
    // constructors
    Matrix() : m_rows{0}, m_cols{0} {}
    Matrix(int rows, int cols) : m_rows{rows}, m_cols{cols}, m_data(rows * cols) {}

    std::vector<double> &vec() { return m_data; }
    const std::vector<double> &vec() const { return m_data; }

    int nr_rows() const { return m_rows; }
    int nr_cols() const { return m_cols; }
    double &operator()(int r, int c) { return m_data[r * m_cols + c]; }
    const double &operator()(int r, int c) const { return m_data[r * m_cols + c]; }

    friend std::istream &operator>>(std::istream &is, Matrix &matrix); // give operator access to private variables
};

/*! Reads a Matrix from 'is' stream. */
std::istream &operator>>(std::istream &is, Matrix &matrix)
{
    std::vector<double> data;
    double num_var = 0;
    int rows = 0;
    std::string temp;
    std::string stringMatrix;

    while (getline(is, temp))
    {
        temp += ", ";
        rows++;
        stringMatrix += temp;
    }

    std::stringstream ss(stringMatrix);
    char junko;

    while (ss >> num_var)
    {
        ss >> junko;

        data.push_back(num_var);
    }

    matrix.m_rows = rows;
    matrix.m_data = data;
    matrix.m_cols = matrix.m_data.size() / rows;

    return is;
}

/*! Writes Matrix 'matrix' to 'os' stream. */
std::ostream &operator<<(std::ostream &os, const Matrix &matrix)
{
    // os << rows+cols;
    return os; // to be completed
}

/*! Returns a new Matrix that is the negation of 'matrix' */
Matrix operator-(const Matrix &matrix)
{
    Matrix newMatrix(matrix.nr_rows(), matrix.nr_cols());

    for (size_t i = 0; i < matrix.vec().size(); ++i)
        newMatrix.vec()[i] = matrix.vec()[i] * -1;

    return newMatrix;
}

/*! Returns a new Matrix that is the transpose of 'matrix' */
Matrix transpose(const Matrix &matrix)
{
    // int rows = matrix.nr_rows();
    // int cols = matrix.nr_cols();
    // Matrix newMatrix(rows, cols);

    // for (size_t i = 0; i < rows; ++i)
    //     for (size_t j = 0; j < cols; ++j)
    //         newMatrix.vec()[i] = newMatrix.vec()[i * j];

    return matrix; // to be completed
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
Matrix operator+(const Matrix &m1, const Matrix &m2)
{

    Matrix newMatrix(m1.nr_rows(), m1.nr_cols());

    for (size_t i = 0; i < m1.vec().size() + 1; i++)
            newMatrix.vec()[i] = m1.vec()[i] + m2.vec()[i];

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
Matrix operator-(const Matrix &m1, const Matrix &m2)
{
    Matrix newMatrix(m1.nr_rows(), m1.nr_cols());

    for (size_t i = 0; i < m1.vec().size() + 1; i++)
            newMatrix.vec()[i] = m1.vec()[i] - m2.vec()[i];

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
Matrix operator*(const Matrix &m1, const Matrix &m2)
{
    return m1; // to be completed
}

#endif
