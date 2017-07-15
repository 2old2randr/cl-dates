# cl-dates

*A comprehensive date handling library for Common Lisp*

This library contains a fairly comprehensive set of functions to
handle dates with somewhat weaker support for date-times. It is useful
for applications where dates are an important data type e.g., business
or financial calculations.

Dates are stored as a floating point number whose value is the Julian
day number plus the day fraction corresponding to the time. Since a
Julian date, by definition starts at noon, the date is an integer only
when the time is exactly noon. For example, "January 1, 2017" can have
values from 2457754.5 to 2457755.499*. As a result, the date
arithmetic functions provided work off the Julian day number (2457755
in this case) and ignore the time.

## Features

*cl-dates* has the following features:

- *Parse a date from a string*: Largely based on the Python
  *dateutils* library, a complex parser for commonly encountered date
  formats is provided in the function **string->date**. The test cases
  in *test-parse-date.lisp* provide a flavour of what kinds of strings
  are acceptable.

- Dates can be *converted to a string* in several formats including
  ISO 8601 and RFC 822 using the function **string->date**.

- Functions are provided to calculate *astronomical dates* for any
  given year (**easter-day**, **vernal-equinox**,
  **autumnal-equinox**, **summer-solstice**, and **winter-solstice**).

- *Date arithmetic*
    - Functions are provided to add or subtract days or months from a
      date (**date+**, **date-**, **add-months**,
      **add-years**). Addition of months or years correctly handles
      cases where the given date falls on a month end.
    - Compute differences between dates (**diff-days**,
      **diff-years**). When computing the number of years, the day
      convention (such as *Actual/365* or *30/360*) is taken into
      account so that the result can be used to accurately compute
      interest on a bond or loan.

- *Miscellaneous* date manipulation functions are provided to compute
  the last day of a month, last day of the previous month, Nth day of
  week in a month (e.g., 3rd Wednesday), etc.

## Dependencies

The only dependency is on the Common Lisp package. There are no third
party library dependencies.

## License

*cl-dates* is licensed under the 2-clause BSD license.

## To Do

Date arithmetic functions that use holidays and weekends for
adjustments are not yet included.
