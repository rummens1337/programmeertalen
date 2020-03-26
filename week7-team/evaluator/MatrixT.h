#ifndef MATRIXT_INCLUDED
#define MATRIXT_INCLUDED

#include <vector>
#include <iostream>

#include "evaluator_exception.h"
#include "evaluator_string_tools.h"

/*! Represents a 2 dimensional matrix of template type T. */
template <typename T>
class MatrixT
{
    int m_rows, m_cols;
    std::vector<T> m_data;

public:
    // constructors
    MatrixT() : m_rows{0}, m_cols{0} {}
    MatrixT(int rows, int cols) : m_rows{rows}, m_cols{cols},
            m_data(rows * cols) {}

    std::vector<T> &vec() { return m_data; }
    const std::vector<T> &vec() const { return m_data; }

    int nr_rows() const { return m_rows; }
    int nr_cols() const { return m_cols; }
    T &operator()(int r, int c) { return m_data[r * m_cols + c]; }
    const T &operator()(int r, int c) const { return m_data[r * m_cols + c]; }

    template <typename T2>
    friend std::istream &operator>>(std::istream &is, MatrixT<T2> &matrix);
    // give operator access to private variables
};

/*! Reads a Matrix from 'is' stream. */
template <typename T>
std::istream &operator>>(std::istream &is, MatrixT<T> &matrix)
{
    std::vector<T> data;
    char char_var; // Toegevoegd.
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
    char junk;

    // Toegevoegd/aangepast VV

    ss >> num_var;

    if (ss.fail())
    {
        while (ss >> char_var)
        {
            ss >> junk;

            Str str;
            // doe iets met char_var;

            data.push_back((T) str);
        }
    }
    else
    {
        ss >> junk; // overbodig?

        data.push_back((T) num_var);

        while (ss >> num_var)
        {
            ss >> junk;

            data.push_back((T) num_var);
        }
    }

    // Toegevoegd/aangepast

    matrix.m_rows = rows;
    matrix.m_data = data;
    matrix.m_cols = matrix.m_data.size() / rows;

    return is;
}

/*! Writes Matrix 'matrix' to 'os' stream. */
template <typename T>
std::ostream &operator<<(std::ostream &os, const MatrixT<T> &matrix)
{
    int cols = matrix.nr_cols();
    std::vector<T> data = matrix.vec();
    std::string output = "";

    for (unsigned int i = 0; i < data.size(); i++)
    {
        output += std::to_string(data[i]);

        if ((i + 1) % cols == 0)
        {
            output += "\n";
        }
        else
        {
            output += ",";
        }
    }

    os << output;

    return os;
}

/*! Returns a new Matrix that is the negation of 'matrix'. */
template <typename T>
MatrixT<T> operator-(const MatrixT<T> &matrix)
{
    MatrixT<T> newMatrix(matrix.nr_rows(), matrix.nr_cols());

    for (size_t i = 0; i < matrix.vec().size(); ++i)
    {
        newMatrix.vec()[i] = matrix.vec()[i] * -1;
    }

    return newMatrix;
}

/*! Returns a new Matrix that is the transpose of 'matrix'. */
template <typename T>
MatrixT<T> transpose(const MatrixT<T> &matrix)
{
    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();
    MatrixT<T> newMatrix(cols, rows);

    int c = 0;
    int count = 0;
    for (signed int i = 0; i < cols; ++i)
    {
        c = 0;
        for (signed int j = 0; j < rows; ++j)
        {
            newMatrix.vec()[count] = matrix.vec()[i + c];
            c += cols;
            count += 1;
        }
    }

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
template <typename T>
MatrixT<T> operator+(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    MatrixT<T> newMatrix(m1.nr_rows(), m1.nr_cols());

    for (size_t i = 0; i < m1.vec().size(); i++)
        newMatrix.vec()[i] = m1.vec()[i] + m2.vec()[i];

    // for (size_t i = 0; i < newMatrix.vec().size(); i++)
    // {
    //     std::cout<<newMatrix.vec()[i]<<"\n";
    // }


    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
template <typename T>
MatrixT<T> operator-(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    MatrixT<T> newMatrix(m1.nr_rows(), m1.nr_cols());

    for (size_t i = 0; i < m1.vec().size(); i++)
        newMatrix.vec()[i] = m1.vec()[i] - m2.vec()[i];

    return newMatrix;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
template <typename T>
MatrixT<T> operator*(const MatrixT<T> &m1, const MatrixT<T> &m2)
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

    MatrixT<T> matrixTransposed = transpose(m2);

    MatrixT<T> newMatrix(rows1, cols2);
    int newSize = rows1 * cols2;
    T total = 0;
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
