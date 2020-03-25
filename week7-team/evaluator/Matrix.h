#ifndef MATRIX_INCLUDED
#define MATRIX_INCLUDED

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

/*! Represents a 2 dimensional matrix of doubles.*/
class Matrix
{
    int m_rows, m_cols;
    std::vector<double> m_data;

public:
    // constructors
    Matrix() : m_rows{0}, m_cols{0} {}
    Matrix(int rows, int cols) : m_rows{rows}, m_cols{cols},
                                 m_data(rows * cols) {}

    std::vector<double> &vec() { return m_data; }
    const std::vector<double> &vec() const { return m_data; }

    int nr_rows() const { return m_rows; }
    int nr_cols() const { return m_cols; }
    double &operator()(int r, int c) { return m_data[r * m_cols + c]; }
    const double &operator()(int r, int c) const
    {
        return m_data[r * m_cols + c];
    }

    friend std::istream &operator>>(std::istream &is, Matrix &matrix);
    // give operator access to private variables
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

    is >> std::fixed;
    is >> std::setprecision(2);

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
    int cols = matrix.nr_cols();
    std::vector<double> data = matrix.vec();

    for (unsigned int i = 0; i < data.size(); i++)
    {
        os << data[i];

        if ((i + 1) % cols == 0)
        {
            os << "\n";
        }
        else
        {
            os << ",";
        }
    }

    return os;
}

/*! Returns a new Matrix that is the negation of 'matrix' */
Matrix operator-(const Matrix &matrix)
{
    Matrix newMatrix(matrix.nr_rows(), matrix.nr_cols());

    for (unsigned int i = 0; i < matrix.vec().size(); i++)
        newMatrix.vec()[i] = matrix.vec()[i] * -1;

    return newMatrix;
}

/*! Returns a new Matrix that is the transpose of 'matrix' */
Matrix transpose(const Matrix &matrix)
{
    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();
    Matrix newMatrix(cols, rows);

    int c = 0;
    int count = 0;
    for (int i = 0; i < cols; i++)
    {
        c = 0;
        for (int j = 0; j < rows; j++)
        {

            newMatrix.vec()[count] = matrix.vec()[i + c];
            c += cols;
            count += 1;
        }
    }

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
Matrix operator+(const Matrix &m1, const Matrix &m2)
{

    Matrix newMatrix(m1.nr_rows(), m1.nr_cols());

    for (unsigned int i = 0; i < m1.vec().size(); i++)
        newMatrix.vec()[i] = m1.vec()[i] + m2.vec()[i];

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
Matrix operator-(const Matrix &m1, const Matrix &m2)
{
    Matrix newMatrix(m1.nr_rows(), m1.nr_cols());

    for (unsigned int i = 0; i < m1.vec().size(); i++)
        newMatrix.vec()[i] = m1.vec()[i] - m2.vec()[i];

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
Matrix operator*(const Matrix &m1, const Matrix &m2)
{
    int rows1 = m1.nr_rows();
    int rows2 = m2.nr_rows();
    int cols1 = m1.nr_cols();
    int cols2 = m2.nr_cols();

    if (cols1 != rows2)
    {
        throw new Evaluator_exception("Invalid dimensions");
    }

    int size = rows1 * cols1;

    Matrix matrixTransposed = transpose(m2);

    Matrix newMatrix(rows1, cols2);
    int newSize = rows1 * cols2;
    double total = 0;
    int i = 0;
    int j = 0;
    int c = 0;

    while (c < newSize)
    {
        total += (m1.vec()[i] * matrixTransposed.vec()[j]);

        i++;
        j++;

        if (i % cols1 == 0)
        {
            newMatrix.vec()[c] = total;
            total = 0;
            c++;
            i -= cols1;

            if (j == size)
            {
                i += cols1;
                j = 0;
            }
        }
    }

    return newMatrix;
}

#endif
