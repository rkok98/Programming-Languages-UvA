/*
 * Author:   René Kok (13671146)
 * Study:    Doorstroomminor Software Engineering UvA
 * 
 * This file defines a custom string object class and the according 
 * respective operator overloaded functions: negation, +, -, *. 
 * This file also includes two functions for reading and writing the custom string object.
 */

#include <string>
#include <sstream>
#include <iostream>

class Str
{
public:
    Str() : m_str("") {}

    Str(std::string string)
    {
        m_str = string;
    }

    std::string str() const
    {
        return m_str;
    }

    friend std::istream &operator>>(std::istream &is, Str &string);

private:
    std::string m_str;
};

/*! Reads a Str object from 'is' stream. */
std::istream &operator>>(std::istream &is, Str &str)
{
    return is >> str.m_str;
}

/*! Writes Str object 'str' to 'os' stream. */
std::ostream &operator<<(std::ostream &os, const Str &str)
{
    return os << str.str();
}

/*! Returns a new Str object that is the negation of 'str'. */
Str operator-(const Str &str)
{
    return Str("(-" + str.str() + ")");
}

/*! Returns a new Str object that is equal to 'str_1+str_2'. */
Str operator+(const Str &str_1, const Str &str_2)
{
    return Str(str_1.str() + "+" + str_2.str());
}

/*! Returns a new Str object that is equal to 'str_1-str_2'. */
Str operator-(const Str &str_1, const Str &str_2)
{
    return Str(str_1.str() + "-" + str_2.str());
}

/*! Returns a new Str object that is equal to 'str_1*str_2'. */
Str operator*(const Str &str_1, const Str &str_2)
{
    return Str("(" + str_1.str() + ")*(" + str_2.str() + ")");
}
