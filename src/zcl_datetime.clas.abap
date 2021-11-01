"! <p class="shorttext synchronized" lang="en">DateTime object</p>
CLASS zcl_datetime DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.


    "! <p class="shorttext synchronized" lang="en">Type of rounding</p>
    TYPES ty_rounding TYPE i .
    TYPES ty_diff TYPE decfloat16 .

    "! <p class="shorttext synchronized" lang="en">Day of week</p>
    TYPES ty_day_of_week TYPE i .
    TYPES:

      "! <p class="shorttext synchronized" lang="en">Structure with reference to CL_DATETIME</p>
      BEGIN OF ty_datetime_element,
        o_datetime TYPE REF TO zcl_datetime,
      END OF ty_datetime_element .
    TYPES:

      "! <p class="shorttext synchronized" lang="en">ISO week</p>
      BEGIN OF ty_iso_week,
        week        TYPE i,
        year        TYPE i,
        day_of_week TYPE ty_day_of_week,
      END OF ty_iso_week .
    TYPES:

      "! <p class="shorttext synchronized" lang="en">TT: List of datetime elements</p>
      ty_t_datetime_list TYPE STANDARD TABLE OF ty_datetime_element WITH EMPTY KEY .

    TYPES:
           "! <p class="shorttext synchronized" lang="en">Fields Date &amp; Time</p>
           ty_datetime_field TYPE string.


    "! <p class="shorttext synchronized" lang="en">Date</p>
    DATA m_date TYPE dats READ-ONLY .

    "! <p class="shorttext synchronized" lang="en">Time</p>
    DATA m_time TYPE time READ-ONLY .

    CONSTANTS:

      "! <p class="shorttext synchronized" lang="en">Fields date &amp; time</p>
      BEGIN OF c_fields,
        "! Second
        second TYPE ty_datetime_field VALUE 'SECOND'  ##NO_TEXT,
        "! Minute
        minute TYPE ty_datetime_field VALUE 'MINUTE'  ##NO_TEXT,
        "! Hour
        hour   TYPE ty_datetime_field VALUE 'HOUR'    ##NO_TEXT,
        "! Day
        day    TYPE ty_datetime_field VALUE 'DAY'     ##NO_TEXT,
        "! Week
        week   TYPE ty_datetime_field VALUE 'WEEK'    ##NO_TEXT,
        "! Month
        month  TYPE ty_datetime_field VALUE 'MONTH'   ##NO_TEXT,
        "! Year
        year   TYPE ty_datetime_field VALUE 'YEAR'    ##NO_TEXT,
      END  OF c_fields .
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Wellknown times</p>
      BEGIN OF c_times,
        "! Begin of day.
        begin_of_day TYPE tims VALUE '000000' ##NO_TEXT,
        "! Noon of day
        noon         TYPE tims VALUE '120000' ##NO_TEXT,
        "! End of day
        end_of_day   TYPE tims VALUE '235959' ##NO_TEXT,
      END OF c_times .
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Enum day of week</p>
      BEGIN OF c_day_of_week,
        "! Monday
        monday    TYPE ty_day_of_week VALUE 1,
        "! Tuesday
        tuesday   TYPE ty_day_of_week VALUE 2,
        "! Wednesday
        wednesday TYPE ty_day_of_week VALUE 3,
        "! Thursday
        thursday  TYPE ty_day_of_week VALUE 4,
        "! Friday
        friday    TYPE ty_day_of_week VALUE 5,
        "! Saturday
        saturday  TYPE ty_day_of_week VALUE 6,
        "! Sunday
        sunday    TYPE ty_day_of_week VALUE 7,
      END OF c_day_of_week .
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Enum months</p>
      BEGIN OF c_month,
        "! January
        january   TYPE i VALUE 1,
        "! February
        february  TYPE i VALUE 2,
        "! March
        march     TYPE i VALUE 3,
        "! April
        april     TYPE i VALUE 4,
        "! May
        may       TYPE i VALUE 5,
        "! June
        june      TYPE i VALUE 6,
        "! July
        july      TYPE i VALUE 7,
        "! August
        august    TYPE i VALUE 8,
        "! September
        september TYPE i VALUE 9,
        "! October
        october   TYPE i VALUE 10,
        "! November
        november  TYPE i VALUE 11,
        "! December
        december  TYPE i VALUE 12,
      END OF c_month .
    "! <p class="shorttext synchronized" lang="en">UTC timezone</p>
    CONSTANTS c_utc TYPE tznzone VALUE 'UTC' ##NO_TEXT.
    CONSTANTS:

      "! <p class="shorttext synchronized" lang="en">Constants for days / years</p>
      BEGIN OF c_day_of_year,
        "! First day of year. Always 1st of January
        first(4)                   TYPE c VALUE '0101',
        "! Last day of year. Always 31st of December
        last(4)                    TYPE c VALUE '1231',
        "! Number of days in a leap year
        max_days_in_leap_year      TYPE i VALUE 366,
        "! Number of days in a none leap year
        max_days_in_none_leap_year TYPE i VALUE 365,
      END OF c_day_of_year .
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Mode rounding</p>
      BEGIN OF c_rouding,
        "!  Round away from zero if value is exactly half
        round_half_up   TYPE ty_rounding VALUE cl_abap_math=>round_half_up,
        "! Round to zero if value is exactly half
        round_half_down TYPE ty_rounding VALUE cl_abap_math=>round_half_down,
        "! Round so last digit is an even no. if value is exactly half
        round_half_even TYPE ty_rounding VALUE cl_abap_math=>round_half_even,
        "!  Round away from zero
        round_up        TYPE ty_rounding VALUE cl_abap_math=>round_up,
        "! Round to Zero
        round_down      TYPE ty_rounding VALUE cl_abap_math=>round_down,
        "! Round to Positive Infinity
        round_ceiling   TYPE ty_rounding VALUE cl_abap_math=>round_ceiling,
        "! Round to Negative Infinity
        round_floor     TYPE ty_rounding VALUE cl_abap_math=>round_floor,
        "! Do not perform any rounding
        round_nothing   TYPE ty_rounding VALUE -1,
      END OF c_rouding .
    CONSTANTS:

      "! <p class="shorttext synchronized" lang="en">General constants for date calculations</p>
      BEGIN OF c_time_constants,
        "! Seconds per day (24 hours)
        num_seconds_per_day         TYPE i          VALUE 86400,
        "! Minutes per day (24 hours)
        num_seconds_per_minute      TYPE i          VALUE 60,
        "! Seconds per hour
        num_seconds_per_hour        TYPE i          VALUE 3600,
        "! Seconds per week
        num_seconds_per_week        TYPE i          VALUE 604800,
        "! Approximate / average seconds per month
        num_seconds_aprox_per_month TYPE decfloat16 VALUE '2628003.6',
        "! Approximate / average seconds per year
        num_seconds_per_year        TYPE i          VALUE 22075200,
        "! Minutes per Hour
        num_minutes_per_hour        TYPE i          VALUE 60,
        "! Minutes per Day (24 hours)
        num_houres_per_day          TYPE i          VALUE 24,
        "! Number of days in a leap year
        num_days_per_leap_year      TYPE i          VALUE 366,
        "! Number of day in a none leap year
        num_days_per_none_leap_year TYPE i          VALUE 365,
      END OF c_time_constants .

    "! <p class="shorttext synchronized" lang="en">Create a list of DateTime objects with an intervall</p>
    "!
    "! @parameter io_start           | <p class="shorttext synchronized" lang="en">Starting at DateTime</p>
    "! @parameter iv_increment_field | <p class="shorttext synchronized" lang="en">Which field should be incremented?</p>
    "! @parameter iv_increment_value | <p class="shorttext synchronized" lang="en">Value to increment</p>
    "! @parameter iv_num_of_elements | <p class="shorttext synchronized" lang="en">Number of passes</p>
    "! @parameter rt_datetime_list   | <p class="shorttext synchronized" lang="en">List of DateTime objects</p>
    CLASS-METHODS create_list
      IMPORTING
        !io_start               TYPE REF TO zcl_datetime
        !iv_increment_field     TYPE ty_datetime_field
        !iv_increment_value     TYPE i
        !iv_num_of_elements     TYPE i
      RETURNING
        VALUE(rt_datetime_list) TYPE ty_t_datetime_list .

    "! <p class="shorttext synchronized" lang="en">Creating an instance based on date &amp; time</p>
    "!
    "! @parameter iv_date                    | <p class="shorttext synchronized" lang="en">Date</p>
    "! @parameter iv_time                    | <p class="shorttext synchronized" lang="en">Time</p>
    "! @raising   cx_parameter_invalid_range | <p class="shorttext synchronized" lang="en">Invalid parameters</p>
    CLASS-METHODS from_date
      IMPORTING
        !iv_date           TYPE dats
        !iv_time           TYPE tims DEFAULT c_times-begin_of_day
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime
      RAISING
        cx_parameter_invalid_range .

    "! <p class="shorttext synchronized" lang="en">Creating an instance based on ISO 8601 timestamp</p>
    "!
    "! @parameter iv_iso8601  | <p class="shorttext synchronized" lang="en">ISO timestamp like 2021-01-01T01:01:01+01:00</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime</p>
    CLASS-METHODS from_iso_timestamp
      IMPORTING
        !iv_iso8601        TYPE string
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Creating an instance based on a timestamp</p>
    "!
    "! @parameter iv_ts       | <p class="shorttext synchronized" lang="en">Timestamp</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    CLASS-METHODS from_long_timestamp
      IMPORTING
        !iv_ts             TYPE timestampl
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Creating an instance based current date &amp; time</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    CLASS-METHODS from_now
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Creating an instance based on a timestamp</p>
    "!
    "! @parameter iv_ts         | <p class="shorttext synchronized" lang="en">Timestamp</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    CLASS-METHODS from_timestamp
      IMPORTING
        !iv_ts               TYPE timestamp
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Creating an instance with current date &amp; time 00:00:00</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    CLASS-METHODS from_today
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Returns max of 2 DateTime objects</p>
    "!
    "! @parameter iv_ignore_time | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    CLASS-METHODS max
      IMPORTING
        !io_datetime_01 TYPE REF TO zcl_datetime
        !io_datetime_02 TYPE REF TO zcl_datetime
        !iv_ignore_time TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_max)   TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Returns min of 2 DateTime objects</p>
    "!
    "! @parameter iv_ignore_time | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    CLASS-METHODS min
      IMPORTING
        !io_datetime_01 TYPE REF TO zcl_datetime
        !io_datetime_02 TYPE REF TO zcl_datetime
        !iv_ignore_time TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_min)   TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Determines of 2 DateTimes the min &amp; max value</p>
    "!
    "! @parameter iv_ignore_time  | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    "! @parameter eo_min_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    "! @parameter eo_max_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    CLASS-METHODS min_max
      IMPORTING
        !io_datetime_01  TYPE REF TO zcl_datetime
        !io_datetime_02  TYPE REF TO zcl_datetime
        !iv_ignore_time  TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !eo_min_datetime TYPE REF TO zcl_datetime
        !eo_max_datetime TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Changes a field of date (e.g Hour, Day...)</p>
    "!
    "! @parameter iv_field    | <p class="shorttext synchronized" lang="en">Fields of DateTime (hour, minute, month...)</p>
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Value (negative values = sub)</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">Instance of DateTime object for method chaining</p>
    METHODS add
      IMPORTING
        !iv_field          TYPE ty_datetime_field
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .


    "! <p class="shorttext synchronized" lang="en">Add days</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of days</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS add_days
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .


    "! <p class="shorttext synchronized" lang="en">Add hours</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of hours</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS add_hours
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Add minutes</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of Minutes</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS add_minutes
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Add month</p>
    METHODS add_months
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Add seconds</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of seconds</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS add_seconds
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Add weeks</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of weeks</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS add_weeks
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Add years</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of years</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS add_years
      IMPORTING
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Returns the date of current DateTime</p>
    "!
    "! @parameter rv_date | <p class="shorttext synchronized" lang="en">Date of current DateTime object</p>
    METHODS as_date
      RETURNING
        VALUE(rv_date) TYPE dats .

    "! <p class="shorttext synchronized" lang="en">Returns a ISO 8601 timestamp of current DateTime</p>
    "!
    "! @parameter rv_iso_timestamp | <p class="shorttext synchronized" lang="en">ISO 8601 timestamp</p>
    METHODS as_iso_timestamp
      RETURNING
        VALUE(rv_iso_timestamp) TYPE string .

    "! <p class="shorttext synchronized" lang="en">Returns a timestamp of current DateTime</p>
    "!
    "! @parameter rv_timestamp | <p class="shorttext synchronized" lang="en">Date &amp; Time as timestamp</p>
    METHODS as_long_timestamp
      RETURNING
        VALUE(rv_timestamp) TYPE timestampl .

    "! <p class="shorttext synchronized" lang="en">Returns a timestamp of current DateTime</p>
    "!
    "! @parameter rv_timestamp | <p class="shorttext synchronized" lang="en">Date &amp; Time as timestamp</p>
    METHODS as_timestamp
      RETURNING
        VALUE(rv_timestamp) TYPE timestamp .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of day</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS begin_of_day
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of month</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS begin_of_month
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of quarter</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS begin_of_quarter
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of week</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS begin_of_week
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of year</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS begin_of_year
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Clones DateTime object to a new instance</p>
    "!
    "! @parameter ro_clone | <p class="shorttext synchronized" lang="en">Clone of current instance</p>
    METHODS clone
      RETURNING
        VALUE(ro_clone) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set day to a day of week</p>
    "!
    "! @parameter pi_day_of_week | <p class="shorttext synchronized" lang="en">Day of week</p>
    "! @parameter pr_o_datetime  | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS day_of_week
      IMPORTING
        !pi_day_of_week      TYPE ty_day_of_week
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Difference between to DateTime objects</p>
    "!
    "! @parameter io_datetime       | <p class="shorttext synchronized" lang="en">DateTime object</p>
    "! @parameter iv_field          | <p class="shorttext synchronized" lang="en">Fields date &amp; time</p>
    "! @parameter iv_round_mode     | <p class="shorttext synchronized" lang="en">Mode of rounding</p>
    "! @parameter iv_round_decimals | <p class="shorttext synchronized" lang="en">Number of decimals for rounding</p>
    "! @parameter rv_diff           | <p class="shorttext synchronized" lang="en">Difference</p>
    METHODS diff
      IMPORTING
        !io_datetime       TYPE REF TO zcl_datetime
        !iv_field          TYPE ty_datetime_field
        !iv_round_mode     TYPE ty_rounding DEFAULT zcl_datetime=>c_rouding-round_nothing
        !iv_round_decimals TYPE i DEFAULT 0
      RETURNING
        VALUE(rv_diff)     TYPE ty_diff .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to end of day</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS end_of_day
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to end of month</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS end_of_month
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of quarter</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS end_of_quarter
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to begin of week</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS end_of_week
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to end of year</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS end_of_year
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Set DateTime to monday of 1st calendar week in year</p>
    "!
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS first_calendar_week
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Returns the century of DateTime</p>
    "!
    "! @parameter rv_century | <p class="shorttext synchronized" lang="en">The century</p>
    METHODS get_century
      RETURNING
        VALUE(rv_century) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Returns the name of day of week</p>
    "!
    "! @parameter rv_dayname | <p class="shorttext synchronized" lang="en">Name of day of week</p>
    METHODS get_dayname
      RETURNING
        VALUE(rv_dayname) TYPE string .

    "! <p class="shorttext synchronized" lang="en">Returns the day of month (1..31)</p>
    "!
    "! @parameter rv_day_of_month | <p class="shorttext synchronized" lang="en">Day in month (1..31)</p>
    METHODS get_day_of_month
      RETURNING
        VALUE(rv_day_of_month) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Returns day of week (1..7)</p>
    "!
    "! @parameter rv_day_of_week | <p class="shorttext synchronized" lang="en">Day of week (1..7)</p>
    METHODS get_day_of_week
      RETURNING
        VALUE(rv_day_of_week) TYPE ty_day_of_week .

    "! <p class="shorttext synchronized" lang="en">Returns the day of year (1..365)</p>
    "!
    "! @parameter rv_day_of_year | <p class="shorttext synchronized" lang="en">Day of year (1..365)</p>
    METHODS get_day_of_year
      RETURNING
        VALUE(rv_day_of_year) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Returns day of week of 1st of january</p>
    "!
    "! @parameter iv_year_offset | <p class="shorttext synchronized" lang="en">Offset Bezugsjahr (+1 next year, -1 previous year)</p>
    "! @parameter rv_doom_day    | <p class="shorttext synchronized" lang="en">Day of week of 1st of januaray</p>
    METHODS get_doom_day
      IMPORTING
        !iv_year_offset    TYPE i DEFAULT 0
      RETURNING
        VALUE(rv_doom_day) TYPE ty_day_of_week .

    "! <p class="shorttext synchronized" lang="en">Returns the gregorian day (days since 1.1.0001)</p>
    "!
    "! @parameter rv_gregorian_day | <p class="shorttext synchronized" lang="en">Gregorian day</p>
    METHODS get_gregorian_day
      RETURNING
        VALUE(rv_gregorian_day) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Returns the ISO Week</p>
    "!
    "! @parameter rs_iso_week | <p class="shorttext synchronized" lang="en">calendar week</p>
    METHODS get_iso_week
      RETURNING
        VALUE(rs_iso_week) TYPE ty_iso_week .

    "! <p class="shorttext synchronized" lang="en">Returns month of DateTime</p>
    METHODS get_month
      RETURNING
        VALUE(rv_month) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Retunrs number of days in month of current DateTime object</p>
    "!
    "! @parameter rv_number_of_days_of_month | <p class="shorttext synchronized" lang="en">Number of days in month</p>
    METHODS get_number_of_days_of_month
      RETURNING
        VALUE(rv_number_of_days_of_month) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Retunrs number of days in year of current DateTime object</p>
    "!
    "! @parameter rv_number_of_days_of_year | <p class="shorttext synchronized" lang="en">Number of days in year</p>
    METHODS get_number_of_days_of_year
      RETURNING
        VALUE(rv_number_of_days_of_year) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Retunrs the quarter of current DateTime object (1..4)</p>
    "!
    "! @parameter rv_quarter | <p class="shorttext synchronized" lang="en">The quarter</p>
    METHODS get_quarter
      RETURNING
        VALUE(rv_quarter) TYPE i .


    "! <p class="shorttext synchronized" lang="en">Returns last day of year</p>
    "!
    "! @parameter rv_last_day_of_month | <p class="shorttext synchronized" lang="en">Last day of months</p>
    METHODS get_ultimo
      RETURNING
        VALUE(rv_last_day_of_month) TYPE dats .

    "! <p class="shorttext synchronized" lang="en">Returns the year of current DateTime object</p>
    "!
    "! @parameter rv_year | <p class="shorttext synchronized" lang="en">The year of date</p>
    METHODS get_year
      RETURNING
        VALUE(rv_year) TYPE i .

    "! <p class="shorttext synchronized" lang="en">Is the other DateTime after this DateTime?</p>
    "!
    "! @parameter io_datetime    | <p class="shorttext synchronized" lang="en">DateTime object</p>
    "! @parameter iv_ignore_time | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    METHODS is_after
      IMPORTING
        !io_datetime       TYPE REF TO zcl_datetime
        !iv_ignore_time    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_after) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Is the other DateTime equal to this DateTime?</p>
    "!
    "! @parameter io_datetime    | <p class="shorttext synchronized" lang="en">DateTime object</p>
    "! @parameter iv_ignore_time | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    METHODS is_after_or_equal
      IMPORTING
        !io_datetime                TYPE REF TO zcl_datetime
        !iv_ignore_time             TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_after_or_equal) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Is the other DateTime before this DateTime?</p>
    "!
    "! @parameter io_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS is_before
      IMPORTING
        !io_datetime        TYPE REF TO zcl_datetime
        !iv_ignore_time     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_is_before) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Is the other DateTime before or equal to this DateTime?</p>
    "!
    "! @parameter io_datetime    | <p class="shorttext synchronized" lang="en">DateTime object</p>
    "! @parameter iv_ignore_time | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    METHODS is_before_or_equal
      IMPORTING
        !io_datetime                 TYPE REF TO zcl_datetime
        !iv_ignore_time              TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_is_before_or_equal) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Is the other DateTime equal this DateTime?</p>
    "!
    "! @parameter io_datetime    | <p class="shorttext synchronized" lang="en">DateTime object</p>
    "! @parameter iv_ignore_time | <p class="shorttext synchronized" lang="en">Ignore time?</p>
    METHODS is_equal
      IMPORTING
        !io_datetime        TYPE REF TO zcl_datetime
        !iv_ignore_time     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_is_before) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Is the day of DateTime th last in month?</p>
    "!
    "! @parameter rv_is_last_day_of_month | <p class="shorttext synchronized" lang="en">Last day of Month?</p>
    METHODS is_last_day_of_month
      RETURNING
        VALUE(rv_is_last_day_of_month) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Is the year a leap year?</p>
    "!
    "! @parameter rv_is_leap | <p class="shorttext synchronized" lang="en">Leap year?</p>
    METHODS is_leap_year
      RETURNING
        VALUE(rv_is_leap) TYPE abap_bool .

    "! <p class="shorttext synchronized" lang="en">Set a Field (day, hour, seconds...)</p>
    "!
    "! @parameter iv_field    | <p class="shorttext synchronized" lang="en">Fields Date &amp; Time</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS set
      IMPORTING
        !iv_field          TYPE ty_datetime_field
        !iv_value          TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Change a Field (day, hour, seconds...)</p>
    "!
    "! @parameter iv_field    | <p class="shorttext synchronized" lang="en">Fields of DateTime (day, hour, second)</p>
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Value (negatives values = add)</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">Instance of current DateTime for method chaining</p>
    METHODS subtract
      IMPORTING
        !iv_field            TYPE ty_datetime_field
        VALUE(iv_value)      TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub days</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of days</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_days
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub hours</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of hours</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_hours
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub Minutes</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of minutes</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_minutes
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub months</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of weeks</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_months
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub seconds</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of seconds</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_seconds
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub weeks</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of weeks</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_weeks
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .

    "! <p class="shorttext synchronized" lang="en">Sub years</p>
    "!
    "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">Number of years</p>
    "! @parameter ro_datetime | <p class="shorttext synchronized" lang="en">DateTime object</p>
    METHODS sub_years
      IMPORTING
        !iv_value            TYPE i
      RETURNING
        VALUE(ro_datetime) TYPE REF TO zcl_datetime .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_ts_string(14) TYPE c .
ENDCLASS.



CLASS zcl_datetime IMPLEMENTATION.


  METHOD add.

    DATA: unit TYPE t006-msehi.

    CASE iv_field.
      WHEN c_fields-second.
        unit = 'S'.
      WHEN c_fields-minute.
        unit = 'MIN'.
      WHEN c_fields-hour.
        unit = 'STD'.
      WHEN c_fields-day.
        unit = 'TAG'.
      WHEN c_fields-week.
        unit = 'WCH'.
      WHEN c_fields-month.
        unit = 'MON'.
      WHEN c_fields-year.
        unit = 'JHR'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid_range
          EXPORTING
            parameter = 'PI_FIELD'
            value     = CONV #( iv_field )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    CONVERT DATE me->m_date TIME me->m_time
        INTO TIME STAMP DATA(ts) TIME ZONE c_utc.

    CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
      EXPORTING
        timestamp_in    = ts
        timezone        = c_utc
        duration        = iv_value
        unit            = unit
      IMPORTING
        timestamp_out   = ts
      EXCEPTIONS
        timestamp_error = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      ##FIXME.
    ENDIF.

    CONVERT TIME STAMP ts TIME ZONE c_utc
      INTO DATE me->m_date
           TIME me->m_time.

    ro_datetime = me.

  ENDMETHOD.


  METHOD add_days.

    ro_datetime = me->add( iv_field = c_fields-day
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD add_hours.

    ro_datetime = me->add( iv_field = c_fields-hour
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD add_minutes.

    ro_datetime = me->add( iv_field = c_fields-minute
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD add_months.

    ro_datetime = me->add( iv_field = c_fields-month
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD add_seconds.

    ro_datetime = me->add( iv_field = c_fields-second
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD add_weeks.

    ro_datetime = me->add( iv_field = c_fields-week
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD add_years.

    ro_datetime = me->add( iv_field = c_fields-year
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD as_date.

    rv_date = me->m_date.

  ENDMETHOD.


  METHOD as_iso_timestamp.

    CONVERT DATE me->m_date TIME me->m_time
     INTO TIME STAMP DATA(ts) TIME ZONE c_utc.

    rv_iso_timestamp = cl_xlf_date_time=>create( ts ).
  ENDMETHOD.


  METHOD as_long_timestamp.

    CONVERT DATE me->m_date TIME me->m_time
            INTO TIME STAMP rv_timestamp TIME ZONE c_utc.

  ENDMETHOD.


  METHOD as_timestamp.

    CONVERT DATE me->m_date TIME me->m_time
            INTO TIME STAMP rv_timestamp TIME ZONE c_utc.

  ENDMETHOD.


  METHOD begin_of_day.

    me->m_time = c_times-begin_of_day.

    ro_datetime = me.

  ENDMETHOD.


  METHOD begin_of_month.

    me->m_date+6(2) = '01'.

    ro_datetime = me.

  ENDMETHOD.


  METHOD begin_of_quarter.

    DATA(quarter) = me->get_quarter( ).

    CASE quarter.
      WHEN 1.
        me->m_date+4(4) = '0101'. ##FIXME.
      WHEN 2.
        me->m_date+4(4) = '0401'.
      WHEN 3.
        me->m_date+4(4) = '0701'.
      WHEN 4.
        me->m_date+4(4) = '1001'.
    ENDCASE.

    ro_datetime = me.

  ENDMETHOD.


  METHOD begin_of_week.

    DATA(current_dow) = me->get_day_of_week( ).

    me->m_date = me->m_date - ( current_dow - 1 ).

    ro_datetime = me.

  ENDMETHOD.


  METHOD begin_of_year.

    me->m_date+4(4) = c_day_of_year-first.

    ro_datetime = me.

  ENDMETHOD.


  METHOD clone.

    ro_clone = NEW #( ).

    ro_clone->m_date = me->m_date.
    ro_clone->m_time = me->m_time.

  ENDMETHOD.


  METHOD create_list.

    CASE iv_increment_field.
      WHEN c_fields-day.
      WHEN c_fields-hour.
      WHEN c_fields-month.
      WHEN c_fields-minute.
      WHEN c_fields-second.
      WHEN c_fields-week.
      WHEN c_fields-year.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid_range
          EXPORTING
            parameter = 'PI_INCREMENT_FIELD'
            value     = CONV #( iv_increment_field )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    IF iv_increment_value <= 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_INCREMENT_VALUE'
          value     = CONV #( iv_increment_value )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    IF iv_num_of_elements < 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_NUM_OF_ELEMENTS'
          value     = CONV #( iv_num_of_elements )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    DATA: loop_count TYPE i VALUE 0.

    DATA(o_current) = io_start->clone( ).

    APPEND INITIAL LINE TO rt_datetime_list ASSIGNING FIELD-SYMBOL(<wa_datetime>).
    <wa_datetime>-o_datetime = o_current.

    o_current = o_current->clone( ).
    loop_count = loop_count + 1.

    WHILE loop_count < iv_num_of_elements.

      o_current->add( iv_field = iv_increment_field
                      iv_value = iv_increment_value ).

      APPEND INITIAL LINE TO rt_datetime_list ASSIGNING <wa_datetime>.
      <wa_datetime>-o_datetime = o_current.

      o_current = o_current->clone( ).

      loop_count = loop_count + 1.

    ENDWHILE.

  ENDMETHOD.


  METHOD day_of_week.

    DATA: current_day_of_week TYPE p,
          delta_in_days       TYPE ty_day_of_week.

    CASE pi_day_of_week.
      WHEN c_day_of_week-monday.
      WHEN c_day_of_week-tuesday.
      WHEN c_day_of_week-wednesday.
      WHEN c_day_of_week-thursday.
      WHEN c_day_of_week-friday.
      WHEN c_day_of_week-sunday.
      WHEN c_day_of_week-saturday.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid_range
          EXPORTING
            parameter = 'PI_DAY_OF_WEEK'
            value     = CONV #( pi_day_of_week )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    current_day_of_week = me->get_day_of_week( ).

    delta_in_days = pi_day_of_week - current_day_of_week.

    me->add( iv_field = c_fields-day
             iv_value = delta_in_days ).

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD diff.

    min_max( EXPORTING io_datetime_01 = me
                       io_datetime_02 = io_datetime
             IMPORTING eo_min_datetime = DATA(o_min)
                       eo_max_datetime = DATA(o_max) ).

    DATA(diff) = cl_abap_tstmp=>subtract( tstmp1 = CONV #( o_max->as_timestamp( ) )
                                          tstmp2 = CONV #( o_min->as_timestamp( ) ) ).

    CASE iv_field.
      WHEN c_fields-day.
        rv_diff = CONV #( diff / c_time_constants-num_seconds_per_day ).
      WHEN c_fields-hour.
        rv_diff = CONV #( diff / c_time_constants-num_seconds_per_hour ).
      WHEN c_fields-minute.
        rv_diff = CONV #( diff / c_time_constants-num_seconds_per_minute ).
      WHEN c_fields-week.
        rv_diff = CONV #( diff / c_time_constants-num_seconds_per_week ).
      WHEN c_fields-year.
        rv_diff = CONV #( diff / c_time_constants-num_seconds_per_year ).
      WHEN c_fields-second.
        rv_diff = diff.
      WHEN c_fields-month.

        IF o_min->is_equal( o_max ).
          rv_diff = 0.
        ELSE.

          DATA(min_year_month)  = ( o_min->get_year( ) * 100 ) + o_min->get_month( ).
          DATA(max_year_month) = ( o_max->get_year( ) * 100 ) + o_max->get_month( ).

          DATA(delta_year_month) = CONV f( max_year_month - min_year_month ).

          DATA(min_ultimo) = o_min->get_ultimo( ).
          DATA(max_ultimo) = o_max->get_ultimo( ).

          DATA(days_om_min) = CONV f( 1 / o_min->get_number_of_days_of_month( ) ).
          DATA(days_om_max) = CONV f( 1 / o_max->get_number_of_days_of_month( ) ).

          DATA(min_day_offset) = days_om_min * ( o_min->get_day_of_month( ) - 1 ). " start day is included
          DATA(max_day_offset) = days_om_max * o_max->get_day_of_month( ).

          DATA(month_offset) = CONV f( min_day_offset  /  ( 1 / max_day_offset ) ).

          rv_diff = CONV #( delta_year_month + ( max_day_offset - min_day_offset ) ).

        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid_range
          EXPORTING
            parameter = 'PI_FIELD'
            value     = CONV #( iv_field )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    IF iv_round_mode NE c_rouding-round_nothing.

      rv_diff = round( val  = rv_diff
                       dec  = iv_round_decimals
                       mode = iv_round_mode ).

    ENDIF.

  ENDMETHOD.


  METHOD end_of_day.

    me->m_time = c_times-end_of_day.

    ro_datetime = me.

  ENDMETHOD.


  METHOD end_of_month.

    DATA(last_of_month) = 0.

    DATA(month) = me->get_month( ).

    CASE month.
      WHEN c_month-january OR
           c_month-march   OR
           c_month-may     OR
           c_month-july    OR
           c_month-august  OR
           c_month-october OR
           c_month-december.
        last_of_month = 31.                              "#EC NUMBER_OK
      WHEN c_month-february.
        " brute check for leap year...
        IF me->is_leap_year( ) = abap_true.
          last_of_month = 29.                            "#EC NUMBER_OK
        ELSE.
          last_of_month = 28.                            "#EC NUMBER_OK
        ENDIF.
      WHEN OTHERS.
        last_of_month = 30.                              "#EC NUMBER_OK
    ENDCASE.

    me->set( iv_field = c_fields-day
             iv_value = last_of_month ).

    ro_datetime = me.

  ENDMETHOD.


  METHOD end_of_quarter.

    ##TODO. " Repository Github A. Teubner
    ##TODO. " Namensraum prüfen.
    ##TODO. " Abhängigkeiten reduzieren.
    ##TODO. " Eigenes Paket.

    DATA(quarter) = me->get_quarter( ).

    CASE quarter.
      WHEN 1.
        me->m_date+4(4) = '0331'.
      WHEN 2.
        me->m_date+4(4) = '0630'.
      WHEN 3.
        me->m_date+4(4) = '0930'.
      WHEN 4.
        me->m_date+4(4) = '1231'.
    ENDCASE.

    ro_datetime = me.

  ENDMETHOD.


  METHOD end_of_week.

    ro_datetime = me.

    DATA(current_dow) = me->get_day_of_week( ).

    me->m_date = me->m_date + ( c_day_of_week-sunday - current_dow ).

    ro_datetime = me.

  ENDMETHOD.


  METHOD end_of_year.

    me->m_date+4(4) = c_day_of_year-last.

    ro_datetime = me.

    ro_datetime = me.

  ENDMETHOD.


  METHOD first_calendar_week.

    DATA(o_date) = me->clone( )->begin_of_year( ).

    DATA(first_day_of_year) = o_date->get_day_of_week( ).

    " Die erste Kalenderwoche eine Jahres, ist die erste Woche,
    " die mindestens vier Tage des neuen Jahre umfasst. Sprich
    " der Erste eines Jahres muss vor dem Freitag liegen.
    IF first_day_of_year > c_day_of_week-thursday.
      me->m_date = o_date->begin_of_week( )->as_date( ).
    ELSE.
      me->m_date = o_date->begin_of_week(
                               )->add_weeks( 1
                               )->as_date( ).
    ENDIF.

    ro_datetime = me.

  ENDMETHOD.


  METHOD from_date.

    " sanity check if date is out of bounds.
    " e.g. 29th february of a none leap year.
    IF CONV i( CONV d( iv_date ) ) = 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_DATE'
          value     = CONV #( iv_date )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    " sanity check if time is out of bounds.
    IF iv_time IS SUPPLIED AND iv_time NOT BETWEEN c_times-begin_of_day AND c_times-end_of_day.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_TIME'
          value     = CONV #( iv_time )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    ro_datetime = NEW #( ).

    ro_datetime->m_date = CONV #( iv_date ).
    ro_datetime->m_time = iv_time.


  ENDMETHOD.


  METHOD from_iso_timestamp.

    DATA(ts) = cl_xlf_date_time=>parse( iv_iso8601 ).

    ro_datetime = NEW #( ).

    CONVERT TIME STAMP ts TIME ZONE c_utc
      INTO DATE ro_datetime->m_date
      TIME ro_datetime->m_time.

  ENDMETHOD.


  METHOD from_long_timestamp.

    ro_datetime = NEW #( ).

    CONVERT TIME STAMP iv_ts TIME ZONE c_utc
      INTO DATE ro_datetime->m_date
      TIME ro_datetime->m_time.

  ENDMETHOD.


  METHOD from_now.

    " Get current date and time...
    GET TIME STAMP FIELD DATA(ts).

    ro_datetime = NEW #( ).

    CONVERT TIME STAMP ts TIME ZONE sy-zonlo
      INTO DATE ro_datetime->m_date
      TIME ro_datetime->m_time.

  ENDMETHOD.


  METHOD from_timestamp.

    pr_o_datetime = NEW #( ).

    CONVERT TIME STAMP iv_ts TIME ZONE c_utc
      INTO DATE pr_o_datetime->m_date
      TIME pr_o_datetime->m_time.

  ENDMETHOD.


  METHOD from_today.

    ro_datetime = NEW #( ).

    ro_datetime->m_date = sy-datum.
    ro_datetime->m_time = c_times-begin_of_day.

  ENDMETHOD.


  METHOD get_century.

    rv_century = CONV i( m_date+0(4) ) DIV 100.

  ENDMETHOD.


  METHOD get_dayname.

    DATA: weekday TYPE dtresr-weekday.
    CALL FUNCTION 'DATE_TO_DAY'
      EXPORTING
        date    = me->m_date
      IMPORTING
        weekday = weekday.

    rv_dayname = weekday.

  ENDMETHOD.


  METHOD get_day_of_month.

    rv_day_of_month = m_date+6(2).

  ENDMETHOD.


  METHOD get_day_of_week.

    DATA(gd) = me->get_gregorian_day( ).

    DATA(mod_gd) = gd MOD 7.

    CASE mod_gd.
      WHEN 0. rv_day_of_week = c_day_of_week-friday.
      WHEN 1. rv_day_of_week = c_day_of_week-saturday.
      WHEN 2. rv_day_of_week = c_day_of_week-sunday.
      WHEN 3. rv_day_of_week = c_day_of_week-monday.
      WHEN 4. rv_day_of_week = c_day_of_week-tuesday.
      WHEN 5. rv_day_of_week = c_day_of_week-wednesday.
      WHEN 6. rv_day_of_week = c_day_of_week-thursday.
    ENDCASE.


  ENDMETHOD.


  METHOD get_day_of_year.

    DATA(first_day) = me->clone( )->begin_of_year( )->as_date( ).

    rv_day_of_year = me->m_date - first_day  + 1.

  ENDMETHOD.


  METHOD get_doom_day.

    rv_doom_day = me->clone(
                    )->begin_of_year(
                    )->add_years( iv_year_offset
                    )->get_day_of_week( ).

  ENDMETHOD.


  METHOD get_gregorian_day.

    CONSTANTS: c_first_day_ad TYPE dats VALUE '00010101'.

    rv_gregorian_day = me->m_date - c_first_day_ad + 1.

  ENDMETHOD.


  METHOD get_iso_week.

    DATA(base_week)           = 0.
    DATA(base_year)           = me->get_year( ).
    DATA(first_dow_this_year) = me->get_doom_day(    ).
    DATA(first_dow_last_year) = me->get_doom_day( -1 ).
    DATA(first_dow_next_year) = me->get_doom_day(  1 ).


    DATA(day_of_year) = me->get_day_of_year( ) - 1.

    IF first_dow_this_year > c_day_of_week-thursday.
      day_of_year = day_of_year  - ( 7 - first_dow_this_year ).
    ELSE.
      day_of_year = day_of_year  + first_dow_this_year.
    ENDIF.

    IF day_of_year < 0.
      IF first_dow_this_year = c_day_of_week-friday OR
            first_dow_last_year = c_day_of_week-thursday.
        base_week = 53.
      ELSE.
        base_week = 52.
      ENDIF.
      base_year = base_year - 1.
    ELSE.
      base_week = ( day_of_year DIV 7 ) + 1.
    ENDIF.

    IF day_of_year > 360 AND base_week > 52.
      IF first_dow_this_year = c_day_of_week-thursday.
        base_week = 53.
      ELSEIF first_dow_next_year = c_day_of_week-friday.
        base_week = 53.
      ELSE.
        base_week = 1.
        base_year = base_year + 1.
      ENDIF.
    ENDIF.

    rs_iso_week-week        = base_week.
    rs_iso_week-year        = base_year.
    rs_iso_week-day_of_week = me->get_day_of_week( ).

  ENDMETHOD.


  METHOD get_month.

    rv_month = m_date+4(2).

  ENDMETHOD.


  METHOD get_number_of_days_of_month.

    DATA(ultimo) = me->get_ultimo( ).
    rv_number_of_days_of_month =  ultimo+6(2).

  ENDMETHOD.


  METHOD get_number_of_days_of_year.

    IF me->is_leap_year( ) = abap_true.
      rv_number_of_days_of_year =  c_day_of_year-max_days_in_leap_year.
    ELSE.
      rv_number_of_days_of_year =  c_day_of_year-max_days_in_none_leap_year.
    ENDIF.

  ENDMETHOD.


  METHOD get_quarter.

    DATA(month) = me->get_month( ).

    DATA(quarter) = CONV f( month / 3 ).

    rv_quarter = round( val = quarter dec = 0 mode = cl_abap_math=>round_ceiling ) .

  ENDMETHOD.


  METHOD get_ultimo.

    DATA(month) = me->get_month( ).

    CASE month.
      WHEN c_month-january  OR
           c_month-march    OR
           c_month-may      OR
           c_month-july     OR
           c_month-august   OR
           c_month-october  OR
           c_month-december.
        rv_last_day_of_month    = me->m_date+0(6) && 31.
      WHEN c_month-february.
        IF me->is_leap_year( ) = abap_true.
          rv_last_day_of_month  = me->m_date+0(6) && 29.
        ELSE.
          rv_last_day_of_month  = me->m_date+0(6) && 28.
        ENDIF.
      WHEN OTHERS.
        rv_last_day_of_month    = me->m_date+0(6) && 30.
    ENDCASE.

  ENDMETHOD.


  METHOD get_year.

    rv_year = m_date+0(4).

  ENDMETHOD.


  METHOD is_after.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( io_datetime->as_timestamp( ) ).

    " Transient time...
    IF iv_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts > other_ts.
      pr_is_after =  abap_true.
    ELSE.
      pr_is_after =  abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_after_or_equal.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( io_datetime->as_timestamp( ) ).

    " Transient time...
    IF iv_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts >= other_ts.
      pr_is_after_or_equal =  abap_true.
    ELSE.
      pr_is_after_or_equal =  abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_before.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( io_datetime->as_timestamp( ) ).

    " Transient time...
    IF iv_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts < other_ts.
      rv_is_before =  abap_true.
    ELSE.
      rv_is_before =  abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD is_before_or_equal.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( io_datetime->as_timestamp( ) ).

    " Transient time...
    IF iv_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts <= other_ts.
      rv_is_before_or_equal =  abap_true.
    ELSE.
      rv_is_before_or_equal =  abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_equal.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( io_datetime->as_timestamp( ) ).

    " Transient time...
    IF iv_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts = other_ts.
      rv_is_before =  abap_true.
    ELSE.
      rv_is_before =  abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD is_last_day_of_month.

    DATA(last_day_of_month) = me->get_ultimo( ).

    rv_is_last_day_of_month = COND #( WHEN me->m_date = last_day_of_month
                                            THEN  abap_true
                                            ELSE  abap_false ).

  ENDMETHOD.


  METHOD is_leap_year.

    DATA(year) = me->get_year( ).

    DATA(mod_4)   = year MOD 4.
    DATA(mod_100) = year MOD 100.
    DATA(mod_400) = year MOD 400.

    IF mod_4 = 0 AND mod_100 NE 0.
      rv_is_leap = abap_true.
    ELSEIF mod_400 = 0.
      rv_is_leap = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD max.

    IF io_datetime_01->is_equal( io_datetime = io_datetime_02 iv_ignore_time = iv_ignore_time ).
      ro_max = io_datetime_01.
      RETURN.
    ENDIF.

    IF io_datetime_01->is_before( io_datetime = io_datetime_02 iv_ignore_time = iv_ignore_time ).
      ro_max = io_datetime_01.
      RETURN.
    ELSE.
      ro_max = io_datetime_02.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD min.

    IF io_datetime_01->is_equal( io_datetime = io_datetime_02 iv_ignore_time = iv_ignore_time ).
      ro_min = io_datetime_02.
      RETURN.
    ENDIF.

    IF io_datetime_01->is_before( io_datetime = io_datetime_02 iv_ignore_time = iv_ignore_time ).
      ro_min = io_datetime_02.
      RETURN.
    ELSE.
      ro_min = io_datetime_01.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD min_max.

    IF io_datetime_01->is_equal( io_datetime = io_datetime_02 iv_ignore_time = iv_ignore_time ).
      eo_min_datetime = io_datetime_01.
      eo_max_datetime = io_datetime_02.
      RETURN.
    ENDIF.

    IF io_datetime_01->is_before( io_datetime = io_datetime_02 iv_ignore_time = iv_ignore_time ).
      eo_min_datetime = io_datetime_01.
      eo_max_datetime = io_datetime_02.
      RETURN.
    ENDIF.

    eo_min_datetime = io_datetime_02.
    eo_max_datetime = io_datetime_01.

  ENDMETHOD.


  METHOD set.

    ro_datetime = me.

    CASE iv_field.
      WHEN c_fields-second.
        IF iv_value < 0 OR iv_value > 59.
          RETURN.
        ENDIF.
        me->m_time+4(2) = |{ iv_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-minute.
        IF iv_value < 0 OR iv_value > 60.
          RETURN.
        ENDIF.
        me->m_time+2(2) = |{ iv_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-hour.
        IF iv_value < 0 OR iv_value > 23.
          RETURN.
        ENDIF.
        me->m_time+0(2) = |{ iv_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-day.
        DATA(number_of_days_in_month) = me->get_number_of_days_of_month( ).
        IF iv_value < 0 OR iv_value > number_of_days_in_month.
          RETURN.
        ENDIF.
        me->m_date+6(2) = |{ iv_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-month.
        IF iv_value < 1 OR iv_value > 12.
          RETURN.
        ENDIF.
        me->m_date+4(2) = |{ iv_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-year.
        IF iv_value < 0 OR iv_value > 9999.
          RETURN.
        ENDIF.
        me->m_date+0(4) = |{ iv_value WIDTH = 4 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-week.
        IF iv_value < 1 OR iv_value > 53.
          RETURN.
        ENDIF.
        DATA(new_date) = me->clone(
                                )->begin_of_year(
                                )->add_weeks( iv_value
                                )->begin_of_week(
                                )->add_days( me->get_day_of_week( ) - 1
                                )->as_date( ).
        me->m_date = new_date.
    ENDCASE.

  ENDMETHOD.


  METHOD subtract.

    iv_value = iv_value * -1.

    ro_datetime = me->add( iv_field = iv_field
                             iv_value = iv_value ).

  ENDMETHOD.


  METHOD sub_days.

    ro_datetime = me->add( iv_field = c_fields-day
                             iv_value = iv_value * -1 ).

  ENDMETHOD.


  METHOD sub_hours.

    ro_datetime = me->add( iv_field = c_fields-hour
                             iv_value = iv_value * -1 ).

  ENDMETHOD.


  METHOD sub_minutes.

    ro_datetime = me->add( iv_field = c_fields-minute
                             iv_value = iv_value * -1 ).

  ENDMETHOD.


  METHOD sub_months.

    ro_datetime = me->add( iv_field = c_fields-month
                             iv_value = iv_value * -1 ).

  ENDMETHOD.


  METHOD sub_seconds.

    ro_datetime = me->add( iv_field = c_fields-second
                             iv_value = iv_value * -1 ).

  ENDMETHOD.


  METHOD sub_weeks.

    ro_datetime = me->add( iv_field = c_fields-week
                             iv_value = iv_value * -1 ).

  ENDMETHOD.


  METHOD sub_years.

    ro_datetime = me->add( iv_field = c_fields-year
                             iv_value = iv_value * -1 ).

  ENDMETHOD.
ENDCLASS.
