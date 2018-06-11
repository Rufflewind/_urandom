from __future__ import division
import os
import yaml

PAY_FREQ_BIWEEKLY = 26
PAY_FREQ_ANNUAL = 1

def load_yaml(path):
    with open(path) as f:
        return yaml.load(f)

PARAMS = load_yaml(os.path.join(os.path.dirname(__file__), "taxes.yaml"))

class BracketedTax(object):
    """Allows accumulated tax calculations."""

    def __init__(self, brackets):
        self._brackets = [(0, 0), (0, 0)] + list(brackets) + [(0, float("inf"))]
        self._index = 0
        self._remaining = 0

    def tax(self, income):
        tax = 0
        assert income >= 0
        while True:
            rate = self._brackets[self._index][0]
            if income <= self._remaining:
                self._remaining -= income
                return tax + income * rate
            income -= self._remaining
            tax += self._remaining * rate
            self._index += 1
            self._remaining = (self._brackets[self._index + 1][1]
                               - self._brackets[self._index][1])

def get_tax_with(brackets, income):
    return BracketedTax(brackets).tax(income)

def map_brackets(brackets, f):
    return [(r, f(x)) for r, x in brackets]

class TaxYear(object):

    def __init__(self, year, params=None):
        self.year = year
        self.params = params or PARAMS

    def __repr__(self):
        return "TaxYear({!r}, {!r})".format(self.year, self.params)

    def with_filing_status(self, filing_status):
        return TaxFiler(self, filing_status)

    def param(self, name):
        return self.params[self.year]["common"][name]

    def supplemental_withholding(self):
        return BracketedTax(
            self.param("fed")["supplemental_withholding_brackets"]
        )

    def social_security_tax(self, income):
        return get_tax_with(self.param("fed")["social_security_brackets"],
                            income)

    def ca_supplemental_withholding_tax(self, income):
        return income * self.param("ca")["supplemental_withholding_rate"]

    def ca_bonus_withholding_surtax(self, income):
        return income * (self.param("ca")["bonus_withholding_rate"]
                         - self.param("ca")["supplemental_withholding_rate"])

    def ca_sdi_withholding(self):
        return BracketedTax(self.param("ca")["sdi_brackets"])

class TaxFiler(object):

    def __init__(self, year, filing_status, params=None):
        self.year = year
        self.filing_status = filing_status
        self.params = params or PARAMS

    def __repr__(self):
        return "TaxFiler({!r}, {!r}, {!r})".format(
            self.year,
            self.filing_status,
            self.params,
        )

    def param(self, name):
        return self.params[self.year][self.filing_status][name]

    def tax_year(self):
        return TaxYear(self.year, self.params)

    def income_tax(self, income):
        return get_tax_with(self.param("fed")["income_brackets"], income)

    def amt(self, income):
        return get_tax_with(self.param("fed")["amt_brackets"], income)

    def regular_withholding_tax(self, wage, pay_freq, num_allowances):
        adjusted_income = max(0, (
            wage * pay_freq
            - self.param("fed")["standard_deduction"]
            - self.tax_year().param("fed")["allowance_deduction"]
            * (num_allowances - self.param("fed")["base_num_allowances"])
        ))
        return get_tax_with(self.param("fed")["income_brackets"],
                            adjusted_income) / pay_freq

    def supplemental_withholding(self):
        return self.tax_year().supplemental_withholding()

    def social_security_tax(self, income):
        return self.tax_year().social_security_tax(income)

    def medicare_tax(self, income):
        return get_tax_with(self.param("fed")["medicare_brackets"], income)

    def fica_taxes(self, income):
        return self.social_security_tax(income) + self.medicare_tax(income)

    def ca_income_tax(self, income):
        return get_tax_with(self.param("ca")["income_brackets"], income)

    def ca_regular_withholding_tax(self, wage, pay_freq, num_allowances):
        if wage * pay_freq <= self.param("ca")["low_income_exemption"]:
            return 0
        adjusted_income = max(
            0,
            wage * pay_freq
            - self.param("ca")["standard_deduction"]
        )
        return max(
            0,
            get_tax_with(self.param("ca")["income_brackets"], adjusted_income)
            - num_allowances * self.tax_year().param("ca")["allowance_credit"]
        ) / pay_freq

    def ca_supplemental_withholding_tax(self, income):
        return self.tax_year().ca_supplemental_withholding_tax(income)

    def ca_bonus_withholding_surtax(self, income):
        return self.tax_year().ca_bonus_withholding_surtax(income)

    def ca_sdi_withholding(self):
        return self.tax_year().ca_sdi_withholding()
