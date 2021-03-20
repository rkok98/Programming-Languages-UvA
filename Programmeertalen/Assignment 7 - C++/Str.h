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

std::istream &operator>>(std::istream &is, Str &str)
{
    return is >> str.m_str;
}

std::ostream &operator<<(std::ostream &os, const Str &str)
{
    return os << str.str();
}

Str operator-(const Str &str)
{
    return Str("(-" + str.str() + ")");
}

Str operator+(const Str &str_1, const Str &str_2)
{
    return Str(str_1.str() + "+" + str_2.str());
}

Str operator-(const Str &str_1, const Str &str_2)
{
    return Str(str_1.str() + "-" + str_2.str());
}

Str operator*(const Str &str_1, const Str &str_2)
{
    return Str("(" + str_1.str() + ")*(" + str_2.str() + ")");
}
