# cl-dates
Date-time library for Common Lisp

This library contains a fairly comprehensive set of functions to handle dates with somewhat weaker support for date-times. It is useful for applications where dates are an important data type e.g., business or financial calculations.

Features provided are:
- Conversion from a string to date: a wide variety of date time formats are accepted (see test-parse-date.lisp for examples)
- Printing dates in various formats
- Computation of special dates / datetimes - Easter, Spring / Autumn equinoxes, Summer / Winter Solstice
- Date arithmetic including support for business dates (holiday processing)
- Date comparisons
- Miscellaneous date functions e.g., computing the 3rd Wednesday of a month

The library does not have any dependencies on third party libraries and is provided under a BSD licence.

*The version currently uploaded is missing all the business date functions but is otherwise usable.*
