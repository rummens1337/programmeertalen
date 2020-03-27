/*
 * Names: Michel Rummens, Thomas Vos
 * StudentIDs:13108093, 12829501
 * This program defines a generic matrix, and the operations +,-,~,* and
 * transpose.
 */

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
    /* Constructors for no data, and for a given amount of rows and cols. */
    MatrixT() : m_rows{0}, m_cols{0} {}
    MatrixT(int rows, int cols) : m_rows{rows}, m_cols{cols},
            m_data(rows * cols) {}

    std::vector<T> &vec() { return m_data; }
    const std::vector<T> &vec() const { return m_data; }

    /* Functions below return and/or change data of the class variables. */
    int nr_rows() const { return m_rows; }
    int nr_cols() const { return m_cols; }
    T &operator()(int r, int c) { return m_data[r * m_cols + c]; }
    const T &operator()(int r, int c) const { return m_data[r * m_cols + c]; }

    template <typename T2>
    friend std::istream &operator>>(std::istream &is, MatrixT<T2> &matrix);
};

/*
 * Reads a Matrix from 'is' stream.
 *
 * @param is Input stream, with a string.
 * @param matrix An empty matrix.
 * @return the now empty input stream.
 */
template <typename T>
std::istream &operator>>(std::istream &is, MatrixT<T> &matrix)
{
    std::vector<T> data;
    T num_var;
    int rows = 0;
    std::string temp;
    std::string stringMatrix;

    /* This reads all input from the istream per line, and constructs it all
    into one string. Also counts the amount of rows. */

    while (getline(is, temp))
    {
        temp += ",";
        rows++;
        stringMatrix += temp;
    }

    std::stringstream ss(stringMatrix);

    /* Code below reads the constructed string for elements with type T, which
    are then placed in a list. To account for string-like types, all the
    whitespace is removed from the string as well, before being processed. */

    while (getline(ss, temp, ','))
    {
        std::stringstream ss2;

        temp.erase(remove(temp.begin(),temp.end(),' '),temp.end());

        ss2<<temp;
        ss2>>num_var;

        data.push_back(num_var);
    }

     /* Lastly, the three class variables of the matrix are defined. */

    matrix.m_rows = rows;
    matrix.m_data = data;
    matrix.m_cols = matrix.m_data.size() / rows;

    return is;
}

/*
 * Writes Matrix 'matrix' to 'os' stream.
 *
 * @param os Output stream, empty at first.
 * @param matrix A matrix, to be put into the output stream.
 * @return the output stream, with the contents of the matrix as a string.
 */
template <typename T>
std::ostream &operator<<(std::ostream &os, const MatrixT<T> &matrix)
{
    int cols = matrix.nr_cols();
    std::vector<T> data = matrix.vec();
    std::string output = "";
    std::string temp;

    /* Loop below Puts every number into the ostream, followed by a comma or
    a newline (if all the numbers of a row have been added). */

    for (unsigned int i = 0; i < data.size(); i++)
    {
        std::stringstream ss;
        ss<<data[i];
        ss>>temp;

        if ((i + 1) % cols == 0)
        {
            output = output + temp + "\n";
        }
        else
        {
            output = output + temp + ",";
        }
    }

    os << output;

    return os;
}

/*
 * Returns a new Matrix that is the negation of 'matrix'.
 *
 * @param matrix A matrix, to be negated.
 * @return the negated matrix.
 */
template <typename T>
MatrixT<T> operator-(const MatrixT<T> &matrix)
{
    MatrixT<T> newMatrix(matrix.nr_rows(), matrix.nr_cols());

    for (size_t i = 0; i < matrix.vec().size(); ++i)
    {
        newMatrix.vec()[i] = -matrix.vec()[i];
    }

    return newMatrix;
}

/*
 * Returns a new Matrix that is the transpose of 'matrix'.
 *
 * @param matrix A matrix, to be transposed.
 * @return the transposed matrix.
 */
template <typename T>
MatrixT<T> transpose(const MatrixT<T> &matrix)
{
    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();
    MatrixT<T> newMatrix(cols, rows);

    int c = 0;
    int count = 0;

    /* The for-loops below put the elements of the original matrix  in the
    correct place of the transposed matrix. */

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

/*
 * Returns a new Matrix that is equal to 'm1+m2'.
 *
 * @param m1/m2 The two matrices that need to be added.
 * @return the matrix that is the result of the addition.
 */
template <typename T>
MatrixT<T> operator+(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    MatrixT<T> newMatrix(m1.nr_rows(), m1.nr_cols());

    for (size_t i = 0; i < m1.vec().size(); i++)
        newMatrix.vec()[i] = m1.vec()[i] + m2.vec()[i];

    return newMatrix;
}

/*
 * Returns a new Matrix that is equal to 'm1-m2'.
 *
 * @param m1/m2 The two matrices that need to be subtracted from each other.
 * @return the matrix that is the result of the subtraction.
 */
template <typename T>
MatrixT<T> operator-(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    MatrixT<T> newMatrix(m1.nr_rows(), m1.nr_cols());

    for (size_t i = 0; i < m1.vec().size(); i++)
        newMatrix.vec()[i] = m1.vec()[i] - m2.vec()[i];

    return newMatrix;
}

/*
 * Returns a new Matrix that is equal to 'm1*m2'.
 *
 * @param m1/m2 The two matrices that need to be multiplied with each other.
 * @return the matrix that is the result of the multiplication.
 */
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

    std::vector<T> totallist;
    T total;

    int i = 0;
    int j = 0;
    int c = 0;

     /* The loop below multiplies every number in a row with every number in a
    column, but rather than immediatly adding all results together, the
    in-between answers are stored in a list.
    If all numbers in a row have been multiplied with a all numbers in a
    column, the final answer is calculated from the list and put into the new
    matrix and the loop will continue to fill the next spots. */

    while (c < newSize)
    {
        totallist.push_back(m1.vec()[i] * matrixTransposed.vec()[j]);

        i++;
        j++;

        if (i % cols1 == 0)
        {
            if (totallist.size() > 0)
            {
                total = totallist[0];
            }

            for (unsigned int i = 1; i < totallist.size(); i++)
            {
                total = total + totallist[i];
            }

            totallist.clear();

            newMatrix.vec()[c] = total;
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
