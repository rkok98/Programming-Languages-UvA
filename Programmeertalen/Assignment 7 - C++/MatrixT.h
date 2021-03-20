#ifndef MATRIXT_INCLUDED
#define MATRIXT_INCLUDED

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

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
    MatrixT(int rows, int cols) : m_rows{rows}, m_cols{cols}, m_data(rows * cols) {}

    std::vector<T> &vec() { return m_data; }
    const std::vector<T> &vec() const { return m_data; }

    int nr_rows() const { return m_rows; }
    int nr_cols() const { return m_cols; }
    T &operator()(int r, int c) { return m_data[r * m_cols + c]; }
    const T &operator()(int r, int c) const { return m_data[r * m_cols + c]; }

    template <typename T2>
    friend std::istream &operator>>(std::istream &is, MatrixT<T2> &matrix); // give operator access to private variables
};

/*! Reads a Matrix from 'is' stream. */
template <typename T>
std::istream &operator>>(std::istream &is, MatrixT<T> &matrix)
{
    std::vector<T> entries;
    std::string row, entry;
    int columns, rows = 0;

    char seperator = ',';

    while (std::getline(is, row))
    {
        rows++;

        std::stringstream stream(row);
        while (std::getline(stream, entry, seperator))
        {
            trim(entry);
            std::istringstream entry_stream(entry);

            T t_entry;
            entry_stream >> t_entry;

            entries.push_back(t_entry);
        }
    }

    columns = entries.size() / rows;

    matrix.m_rows = rows;
    matrix.m_cols = columns;
    matrix.m_data = entries;

    return is;
}

/*! Writes Matrix 'matrix' to 'os' stream. */
template <typename T>
std::ostream &operator<<(std::ostream &os, const MatrixT<T> &matrix)
{
    unsigned int rows = matrix.nr_rows(), columns = matrix.nr_cols();

    for (unsigned int row = 0; row < rows; row++)
    {
        for (unsigned int column = 0; column < columns; column++)
        {
            os << matrix(row, column);

            if (column < columns - 1)
            {
                os << ',';
            }
        }

        if (row < rows - 1)
        {
            os << "\n";
        }
    }

    return os;
}

/*! Returns a new Matrix that is the negation of 'matrix'. */
template <typename T>
MatrixT<T> operator-(const MatrixT<T> &matrix)
{
    MatrixT<T> new_matrix(matrix.nr_rows(), matrix.nr_cols());

    for (unsigned int i = 0; i < matrix.vec().size(); i++)
    {
        new_matrix.vec()[i] = -matrix.vec()[i];
    }

    return new_matrix;
}

/*! Returns a new Matrix that is the transpose of 'matrix'. */
template <typename T>
MatrixT<T> transpose(const MatrixT<T> &matrix)
{
    unsigned int rows = matrix.nr_rows(), columns = matrix.nr_cols();

    MatrixT<T> new_matrix(columns, rows);
    std::vector<T> new_entries;

    for (unsigned int column = 0; column < columns; column++)
    {
        for (unsigned int row = 0; row < rows; row++)
        {
            new_entries.push_back(matrix(row, column));
        }
    }

    new_matrix.vec() = new_entries;

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
template <typename T>
MatrixT<T> operator+(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    MatrixT<T> new_matrix(m1.nr_rows(), m1.nr_cols());

    for (unsigned int i = 0; i < m1.vec().size(); i++)
    {
        new_matrix.vec()[i] = m1.vec()[i] + m2.vec()[i];
    }

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
template <typename T>
MatrixT<T> operator-(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    MatrixT<T> new_matrix(m1.nr_rows(), m1.nr_cols());

    for (unsigned int i = 0; i < m1.vec().size(); i++)
    {
        new_matrix.vec()[i] = m1.vec()[i] + -m2.vec()[i];
    }

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
template <typename T>
MatrixT<T> operator*(const MatrixT<T> &m1, const MatrixT<T> &m2)
{
    unsigned int m1_rows = m1.nr_rows(), m1_columns = m1.nr_cols(),
                 m2_rows = m2.nr_rows(), m2_columns = m2.nr_cols();

    MatrixT<T> new_matrix(m1_rows, m2_columns);
    std::vector<T> new_data;

    T sum{};

    if (m1_columns != m2_rows)
    {
        throw Evaluator_exception("Incorrect dimensions");
    }

    for (unsigned int m1_row = 0; m1_row < m1_rows; m1_row++)
    {
        for (unsigned int m2_column = 0; m2_column < m2_columns; m2_column++)
        {
            sum = m1(m1_row, 0) * m2(0, m2_column);

            for (unsigned int m1_column = 1; m1_column < m1_columns; m1_column++)
            {
                sum = sum + m1(m1_row, m1_column) * m2(m1_column, m2_column);
            }

            new_data.push_back(sum);
        }
    }

    new_matrix.vec() = new_data;

    return new_matrix;
}

#endif
