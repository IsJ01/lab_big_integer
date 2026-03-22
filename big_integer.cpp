#include <iostream>
#include "big_integer.h"
#include <algorithm>

// ======================== Constructors ========================

BigInteger::BigInteger() {
    this->digits_.push_back(0);
}

BigInteger::BigInteger(int value) {
    std::string str = std::to_string(value);
    for (const char& digit: str) {
        if (digit == '-') {
            negative_ = true;
            continue;
        }
        digits_.push_back(digit - '0');
    }
    auto it = digits_.begin();
    while (*it == 0 && digits_.size() > 1) {
        digits_.erase(it);
    }
    if (digits_.size() == 1 && digits_.at(0) == 0) {
        negative_ = false;
    }
}

BigInteger::BigInteger(long long value) {
    std::string str = std::to_string(value);
    for (const char& digit: str) {
        if (digit == '-') {
            negative_ = true;
            continue;
        }
        digits_.push_back(digit - '0');
    }
    auto it = digits_.begin();
    while (*it == 0 && digits_.size() > 1) {
        digits_.erase(it);
    }
    if (digits_.size() == 1 && digits_.at(0) == 0) {
        negative_ = false;
    }
}

BigInteger::BigInteger(const std::string& str) {
    for (const char& digit: str) {
        if (digit == '-') {
            negative_ = true;
            continue;
        }
        digits_.push_back(digit - '0');
    }
    auto it = digits_.begin();
    while (*it == 0 && digits_.size() > 1) {
        digits_.erase(it);
    }
    if (digits_.size() == 1 && digits_.at(0) == 0) {
        negative_ = false;
    }
}

// ==================== Arithmetic ==============================

BigInteger BigInteger::operator+ (const BigInteger& rhs) const {
    BigInteger newInt = *this;
    newInt += rhs;
    return newInt;
}

BigInteger  BigInteger::operator- (const BigInteger& rhs) const {
    BigInteger newInt = *this;
    newInt -= rhs;
    return newInt;
}

BigInteger  BigInteger::operator* (const BigInteger& rhs) const {
    BigInteger newInt = *this;
    newInt *= rhs;
    return newInt;
}

BigInteger  BigInteger::operator/ (const BigInteger& rhs) const {
    BigInteger newInt = *this;
    newInt /= rhs;
    return newInt;
}

BigInteger  BigInteger::operator% (const BigInteger& rhs) const {
    BigInteger newInt = *this;
    newInt %= rhs;
    return newInt;
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    std::vector<int> new_vec;
    bool difference = this->negative_ != rhs.negative_;
    int max_len = std::max(this->digits_.size(), rhs.digits_.size());
    if (!difference) {
        std::vector<int> min_vec;
        std::vector<int> max_vec;
        int carry = 0;

        if (this->digits_.size() > rhs.digits_.size()) {
            max_vec = this->digits_;
            min_vec = rhs.digits_;
        }
        else {
            max_vec = rhs.digits_;
            min_vec = this->digits_;
        }
        std::reverse(min_vec.begin(), min_vec.end());
        while (min_vec.size() != max_vec.size()) {
            min_vec.push_back(0);
        }
        std::reverse(min_vec.begin(), min_vec.end());

        for (int  i = max_len - 1; i > -1; i--) {
            if (max_vec.at(i) + min_vec.at(i) + carry > 9) {
                new_vec.push_back((max_vec.at(i) + min_vec.at(i) + carry) % 10);
                if (i == 0) {
                    new_vec.push_back(1);
                }
                carry = 1;
            } else {
                new_vec.push_back(max_vec.at(i) + min_vec.at(i) + carry);
                carry = 0;
            }
        }
    } else {
        std::vector<int> min_vec;
        std::vector<int> max_vec;
        bool is_negative;

        if (this->digits_.size() > rhs.digits_.size()) {
            max_vec = this->digits_;
            min_vec = rhs.digits_;
            if (this->negative_) is_negative = true;
            else is_negative = false;
        }
        else if (this->digits_.size() < rhs.digits_.size()) {
            max_vec = rhs.digits_;
            min_vec = this->digits_;
            is_negative = rhs.negative_;
            if (rhs.negative_) is_negative = false;
            else is_negative = true;
        } else {
            BigInteger abs_t = *this;
            BigInteger abs_r = rhs;
            if (this->negative_) abs_t = -abs_t;
            if (rhs.negative_) abs_r = -abs_r;
            if (abs_t > abs_r) {
                is_negative = this->negative_;
                max_vec = this->digits_;
                min_vec = rhs.digits_; 
            } else {
                is_negative = rhs.negative_;
                max_vec = rhs.digits_;
                min_vec = this->digits_; 
            }
        }
        std::reverse(min_vec.begin(), min_vec.end());
        while (min_vec.size() != max_vec.size()) {
            min_vec.push_back(0);
        }
        std::reverse(min_vec.begin(), min_vec.end());

        for (int  i = max_len - 1; i > -1; i--) {
            if (max_vec.at(i) - min_vec.at(i) < 0) {
                if (i == 0) {
                    new_vec.push_back(abs(max_vec.at(i) - min_vec.at(i)));
                    break;
                }
                new_vec.push_back(10 + max_vec.at(i) - min_vec.at(i));
                max_vec.at(i - 1)--;
            } else {
                new_vec.push_back(max_vec.at(i) - min_vec.at(i));
            }
        }
        this->negative_ = is_negative;
    }
    std::reverse(new_vec.begin(), new_vec.end());
    auto it = new_vec.begin();
    while (*it == 0 && new_vec.size() > 1) {
        new_vec.erase(it);
    }
    this->digits_ = new_vec;
    if (is_zero()) this->negative_ = false;
    return *this;
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    *this += -rhs;
    return *this;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    this->negative_ = this->negative_ != rhs.negative_;
    BigInteger result;
    for (int  i = rhs.digits_.size() - 1; i > -1; i--) {
        BigInteger new_result;
        std::vector<int> new_vec;
        int carry = 0;
        int r_v = rhs.digits_.at(i);
        for (int  j = this->digits_.size() - 1; j > -1; j--) {
            int t_v = this->digits_.at(j);
            int r = r_v * t_v + carry;
            if (r > 9) {
                new_vec.push_back(r % 10);
                carry = r / 10;
                if (j == 0 && carry > 0) {
                    new_vec.push_back(carry);
                    break;
                }
            }
            else {
                new_vec.push_back(r);
                carry = 0;
            }
        }
        std::reverse(new_vec.begin(), new_vec.end());
        for (int  j = 0; j < static_cast<int>(rhs.digits_.size()) - 1 - i; j++) {
            new_vec.push_back(0);
        }
        new_result.digits_ = new_vec;
        result += new_result;
    }
    this->digits_ = result.digits_;
    return *this;
} 

BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    this->negative_ = this->negative_ != rhs.negative_;
    std::vector<int> new_vec;
    std::string numerator = "";
    for (int i = 0; i <static_cast<int>( this->digits_.size()); i++) {
        numerator += std::to_string(this->digits_.at(i));
        if (numerator == "0") {
            numerator = "";
            new_vec.push_back(0);
            continue;
        };
        BigInteger n(numerator);
        BigInteger abs_rhs = rhs;
        if (abs_rhs.is_negative()) abs_rhs = -abs_rhs;
        if (n >= abs_rhs) {
            for (int j = 9; j > 0; j--) {
                BigInteger res = abs_rhs * BigInteger(j);
                if (res <= n) {
                    new_vec.push_back(j);
                    numerator = (n - res).to_string();
                    if (numerator == "0") numerator = "";
                    break;
                }
            }
        } else if (n < abs_rhs && i == static_cast<int>(this->digits_.size()) - 1) {
            new_vec.push_back(0);
        }
    }
    if (new_vec.size() == 0) {
        new_vec.push_back(0);
        this->negative_ = false;
    }
    this->digits_ = new_vec;
    return *this;
} 

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    std::vector<int> new_vec;
    BigInteger res = *this / rhs;
    this->digits_ = (*this - res * rhs).digits_;
    return *this;
}

// ====================== Unary ================================

BigInteger BigInteger::operator-() const {
    if (is_zero()) {
        return BigInteger();
    }
    std::string new_digit = to_string();
    if (new_digit.at(0) == '-') {
        return BigInteger(new_digit.substr(1));
    }
    return BigInteger("-" + new_digit);

}

BigInteger& BigInteger::operator++() {
    std::vector<int> new_digits;
    if (!negative_) {
        for (int  i = digits_.size() - 1; i > -1; i--) {
            if (digits_.at(i) == 9) {
                digits_.at(i) = 0;
                if (i == 0) {
                    digits_.insert(digits_.begin(), 1);
                }
            }
            else if (digits_.at(i) < 9) {
                digits_.at(i)++;
                break;
            } 
        }
    } else {
        int state = 0;
        if (is_zero()) {
            ++digits_.at(0);
            negative_ = true;
            return *this;
        }
        for (int  i = digits_.size() - 1; i > -1; i--) {
            if (digits_.at(i) == 0 && state == 0) {
                new_digits.insert(new_digits.begin(), 9);
            }
            else if (digits_.at(i) > 0 && state == 0) {
                state = 1;
                new_digits.insert(new_digits.begin(), digits_.at(i) - 1);
            }
            else if (state == 1) {
                new_digits.insert(new_digits.begin(), digits_.at(i));
            }
        }
        auto it = new_digits.begin();
        while (*it == 0 && new_digits.size() > 1) {
            new_digits.erase(it);
        }
        this->digits_ = new_digits;
        if (is_zero()) {
            negative_ = false;
        }
    }
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger new_digit(*this);
    ++(*this);
    return new_digit;
}    

BigInteger& BigInteger::operator--() {
    std::vector<int> new_digits;
    if (negative_) {
        for (int  i = digits_.size() - 1; i > -1; i--) {
            if (digits_.at(i) == 9) {
                digits_.at(i) = 0;
                if (i == 0) {
                    digits_.insert(digits_.begin(), 1);
                }
            }
            else if (digits_.at(i) < 9) {
                digits_.at(i)++;
                break;
            } 
        }
    } else {
        int state = 0;
        if (is_zero()) {
            ++digits_.at(0);
            negative_ = true;
            return *this;
        }
        for (int  i = digits_.size() - 1; i > -1; i--) {
            if (digits_.at(i) == 0 && state == 0) {
                new_digits.insert(new_digits.begin(), 9);
            }
            else if (digits_.at(i) > 0 && state == 0) {
                state = 1;
                new_digits.insert(new_digits.begin(), digits_.at(i) - 1);
            }
            else if (state == 1) {
                new_digits.insert(new_digits.begin(), digits_.at(i));
            }
        }
        auto it = new_digits.begin();
        while (*it == 0 && new_digits.size() > 1) {
            new_digits.erase(it);
        }
        this->digits_ = new_digits;
        if (is_zero()) {
            negative_ = false;
        }
    }
    return *this;
}          

BigInteger  BigInteger::operator--(int) {
    BigInteger new_digit(*this);
    --(*this);
    return new_digit;
}                       

// ======================= Comparison ==========================

bool BigInteger::operator==(const BigInteger& rhs) const {
    if (this->negative_ != rhs.negative_) return false;
    if (this->digits_.size() != rhs.digits_.size()) return false;
    for (int i = 0; i < static_cast<int>(this->digits_.size()); i++) {
        if (this->digits_.at(i) != rhs.digits_.at(i)) return false;
    }
    return true;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator< (const BigInteger& rhs) const {
    if (this->negative_ && !rhs.negative_) return true;
    if (negative_ && this->digits_.size() > rhs.digits_.size()) return true;
    if (!negative_ && this->digits_.size() > rhs.digits_.size()) return false;
    if (negative_ && this->digits_.size() < rhs.digits_.size()) return false;
    if (!negative_ && this->digits_.size() < rhs.digits_.size()) return true;

    bool result = false;
    for (int  i = 0; i < static_cast<int>(this->digits_.size()); i++) {
        if (this->digits_.at(i) == rhs.digits_.at(i)) continue;
        if (this->digits_.at(i) < rhs.digits_.at(i)) {
            result = true;
            break;
        } else {
            result = false;
            break;
        };
    }
    if (!result) {
        if (this->negative_ && rhs.negative_) return true;
        return false;
    } else {
        if (this->negative_ && rhs.negative_) return false;
        return true;
    }
}

bool BigInteger::operator> (const BigInteger& rhs) const {
    return rhs < *this;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return !(*this > rhs);
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

// ======================== Misc ===============================

std::string BigInteger::to_string() const {
    std::string str_digit = "";
    if (negative_) {
        str_digit = "-";
    }
    for (const int& digit: digits_) {
        str_digit += std::to_string(digit);;
    }
    return str_digit;
}

bool BigInteger::is_zero() const {
    return digits_.at(0) == 0;
}

bool BigInteger::is_negative() const {
    return negative_;
} 

BigInteger::operator bool() const {
    return !is_zero();
}

// ======================== I/O ================================
std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    return os << value.to_string();
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string new_val;
    is >> new_val;
    std::vector<int> new_vec;
    for (const char& digit: new_val) {
        if (digit == '-') {
            value.negative_ = true;
            continue;
        }
        new_vec.push_back(digit - '0');
    }
    auto it = new_vec.begin();
    while (*it == 0 && new_vec.size() > 1) {
        new_vec.erase(it);
    }
    value.digits_ = new_vec;
    return is;
}
