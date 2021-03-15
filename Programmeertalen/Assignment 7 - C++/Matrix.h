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
    std::vector<double> double_entries;
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
            double_entries.push_back(atof(entry.c_str()));
        }
    }

    columns = double_entries.size() / rows;

    matrix.m_rows = rows;
    matrix.m_cols = columns;
    matrix.m_data = double_entries;

    return is;
}

/*! Writes Matrix 'matrix' to 'os' stream. */
std::ostream &operator<<(std::ostream &os, const Matrix &matrix)
{
    int rows = matrix.nr_rows(), cols = matrix.nr_cols();

    for (int row = 0; row < rows; row++)
    {
        for (int col = 0; col < cols; col++)
        {
            os << matrix(row, col);

            if (col < cols - 1)
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

/*! Returns a new Matrix that is the negation of 'matrix' */
Matrix operator-(const Matrix &matrix)
{
    Matrix new_matrix(matrix.nr_rows(), matrix.nr_cols());

    for (unsigned int i = 0; i < matrix.vec().size(); i++)
    {
        new_matrix.vec()[i] = matrix.vec()[i] * -1;
    }

    return new_matrix;
}

/*! Returns a new Matrix that is the transpose of 'matrix' */
Matrix transpose(const Matrix &matrix)
{
    std::vector<double> new_entries;
    int rows = matrix.nr_rows(), columns = matrix.nr_cols();

    for (int col = 0; col < columns; col++) {
        for (int row = 0; row < rows; row++) {
            new_entries.push_back(matrix(row, col));
        }
    }

    Matrix new_matrix(columns, rows);
    new_matrix.vec() = new_entries;

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
Matrix operator+(const Matrix &m1, const Matrix &m2)
{
    Matrix new_matrix(m1.nr_rows(), m1.nr_cols());

    for (unsigned int i = 0; i < m1.vec().size(); i++)
    {
        new_matrix.vec()[i] = m1.vec()[i] + m2.vec()[i];
    }

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
Matrix operator-(const Matrix &m1, const Matrix &m2)
{
    Matrix new_matrix(m1.nr_rows(), m1.nr_cols());

    for (unsigned int i = 0; i < m1.vec().size(); i++)
    {
        new_matrix.vec()[i] = m1.vec()[i] - m2.vec()[i];
    }

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
Matrix operator*(const Matrix &m1, const Matrix &m2)
{
    std::vector<double> new_data;
    int m1_rows = m1.nr_rows(), m1_columns = m1.nr_cols(), m2_rows = m2.nr_rows(), m2_cols = m2.nr_cols();
    double sum{};

    if (m1_columns != m2_rows) {
        throw Evaluator_exception("Invalid dimensions"
                                  " in matrix multiplication");
    }

    for (int m1_row = 0; m1_row < m1_rows; m1_row++) {
        for (int m2_col = 0; m2_col < m2_cols; m2_col++) {
            sum = m1(m1_row, 0) * m2(0, m2_col);
            
            for (int m1_col = 1; m1_col < m1_columns; m1_col++) {
                sum = sum + m1(m1_row, m1_col) * m2(m1_col, m2_col);
            }

            new_data.push_back(sum);
        }
    }

    Matrix new_matrix(m1_rows, m2_cols);
    new_matrix.vec() = new_data;

    return new_matrix;
}

#endif
