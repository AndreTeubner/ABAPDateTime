"! <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
CLASS zcl_datetime DEFINITION
  PUBLIC
  FINAL
  CREATE PROTECTED .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="de">Art Rundung</p>
    TYPES ty_rounding TYPE i .
    TYPES ty_diff TYPE decfloat16 .
    "! <p class="shorttext synchronized" lang="de">Wochentag</p>
    TYPES ty_day_of_week TYPE i .
    TYPES:
      "! <p class="shorttext synchronized" lang="de">Struktur mit Referenz auf CL_DATETIME</p>
      BEGIN OF ty_datetime_element,
        o_datetime TYPE REF TO zcl_datetime,
      END OF ty_datetime_element .
    TYPES:
      "! <p class="shorttext synchronized" lang="de">ISO-Woche</p>
      BEGIN OF ty_iso_week,
        week        TYPE i,
        year        TYPE i,
        day_of_week TYPE ty_day_of_week,
      END OF ty_iso_week .
    TYPES:
      "! <p class="shorttext synchronized" lang="de">TT: List of datetime elements</p>
      ty_t_datetime_list TYPE STANDARD TABLE OF ty_datetime_element WITH EMPTY KEY .
    "! <p class="shorttext synchronized" lang="de">Felder Datum &amp; Uhrzeit</p>
    TYPES: ty_datetime_field TYPE string.

    "! <p class="shorttext synchronized" lang="de">Date of object</p>
    DATA ca_date TYPE dats READ-ONLY .
    "! <p class="shorttext synchronized" lang="de">Time of object</p>
    DATA ca_time TYPE time READ-ONLY .
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="de">Fields date & time</p>
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
      "! <p class="shorttext synchronized" lang="de">Wellknown times</p>
      BEGIN OF c_times,
        "! Begin of day.
        begin_of_day TYPE tims VALUE '000000' ##NO_TEXT,
        "! Noon of day
        noon         TYPE tims VALUE '120000' ##NO_TEXT,
        "! End of day
        end_of_day   TYPE tims VALUE '235959' ##NO_TEXT,
      END OF c_times .
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="de">Auflistung Wochentage</p>
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
      "! <p class="shorttext synchronized" lang="de">Auflistung Monate</p>
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
    "! <p class="shorttext synchronized" lang="de">UTC timezone</p>
    CONSTANTS c_utc TYPE tznzone VALUE 'UTC' ##NO_TEXT.
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="de">Constants for days / years</p>
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
      "! <p class="shorttext synchronized" lang="de">Mode rounding</p>
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
      "! <p class="shorttext synchronized" lang="de">General constants for date calculations</p>
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

    "! <p class="shorttext synchronized" lang="de">Create a list of DateTime objects</p>
    "!
    "! The function generate a list of objects, which are created in intervals and have
    "! the respective distance to the predecessor.
    "!
    "! @parameter pi_o_start         | <p class="shorttext synchronized" lang="de">Starting at DateTime/p>
    "! @parameter pi_increment_field | <p class="shorttext synchronized" lang="de">Which field should be incremented?</p>
    "! @parameter pi_increment_value | <p class="shorttext synchronized" lang="de">Value to increment</p>
    "! @parameter pi_num_of_elements | <p class="shorttext synchronized" lang="de">Number of passes</p>
    "! @parameter pr_t_datetime_list | <p class="shorttext synchronized" lang="de">List of DateTime objects</p>
    CLASS-METHODS create_list
      IMPORTING
        !pi_o_start               TYPE REF TO zcl_datetime
        !pi_increment_field       TYPE ty_datetime_field
        !pi_increment_value       TYPE i
        !pi_num_of_elements       TYPE i
      RETURNING
        VALUE(pr_t_datetime_list) TYPE ty_t_datetime_list .
    "! <p class="shorttext synchronized" lang="de">Creating an instance based on date &amp; time</p>
    "!
    "! @parameter pi_date                    | <p class="shorttext synchronized" lang="de">Date</p>
    "! @parameter pi_time                    | <p class="shorttext synchronized" lang="de">Time</p>
    "! @parameter pr_o_datetime              | <p class="shorttext synchronized" lang="de">DateTime</p>
    "! @raising   cx_parameter_invalid_range | <p class="shorttext synchronized" lang="de">Invalid parameters</p>
    CLASS-METHODS from_date
      IMPORTING
        !pi_date             TYPE dats
        !pi_time             TYPE tims DEFAULT c_times-begin_of_day
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime
      RAISING
        cx_parameter_invalid_range .
    "! <p class="shorttext synchronized" lang="de">Creating an instance based on ISO 8601 timestamp</p>
    "!
    "! @parameter pi_iso8601    | <p class="shorttext synchronized" lang="de">ISO timestamp like 2021-01-01T01:01:01+01:00</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">DateTime</p>
    CLASS-METHODS from_iso_timestamp
      IMPORTING
        !pi_iso8601          TYPE string
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Creating an instance based on a timestamp</p>
    "!
    "! @parameter pi_ts         | <p class="shorttext synchronized" lang="de">Timestamp</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    CLASS-METHODS from_long_timestamp
      IMPORTING
        !pi_ts               TYPE timestampl
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Creating an instance based current date &amp; time</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    CLASS-METHODS from_now
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Instantiierung mit einem Timestamp</p>
    "!
    "! @parameter pi_ts         | <p class="shorttext synchronized" lang="de">Timestamp</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    CLASS-METHODS from_timestamp
      IMPORTING
        !pi_ts               TYPE timestamp
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Instantierung aktuellem Datum &amp; Uhrzeit 00:00:00</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    CLASS-METHODS from_today
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Bestimmt  den maximalen Wert</p>
    "!
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    CLASS-METHODS max
      IMPORTING
        !pi_o_datetime_01 TYPE REF TO zcl_datetime
        !pi_o_datetime_02 TYPE REF TO zcl_datetime
        !pi_ignore_time   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_o_max)   TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Bestimmt  den minimalen Wert</p>
    "!
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    CLASS-METHODS min
      IMPORTING
        !pi_o_datetime_01 TYPE REF TO zcl_datetime
        !pi_o_datetime_02 TYPE REF TO zcl_datetime
        !pi_ignore_time   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_o_min)   TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Bestimmt zw. 2 DateTime Objekten den min. &amp; max. Wert</p>
    "!
    "! @parameter pi_ignore_time    | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    "! @parameter pe_o_min_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pe_o_max_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    CLASS-METHODS min_max
      IMPORTING
        !pi_o_datetime_01  TYPE REF TO zcl_datetime
        !pi_o_datetime_02  TYPE REF TO zcl_datetime
        !pi_ignore_time    TYPE abap_bool DEFAULT abap_true
      EXPORTING
        !pe_o_min_datetime TYPE REF TO zcl_datetime
        !pe_o_max_datetime TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Ändern eines Felds (Stunde, Tag etc.) im Datum</p>
    "!
    "! @parameter pi_field      | <p class="shorttext synchronized" lang="de">Feld im Datum (Stunde, Tag etc.)</p>
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Wert (negative Wert = Subtraktion)</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Instanz Datetime-Objekt für Method Chaining</p>
    METHODS add
      IMPORTING
        !pi_field            TYPE ty_datetime_field
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Tage</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Tage</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_days
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Stunden</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Stunden</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_hours
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Minuten</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Minuten</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_minutes
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Monate</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Wochen</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_months
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Sekunden</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Sekunden</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_seconds
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Wochen</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Wochen</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_weeks
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Hinzufügen Jahre</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Jahre</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS add_years
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Liefert das Datum für das aktuelle Datetime-Object</p>
    "!
    "! @parameter pr_date | <p class="shorttext synchronized" lang="de">Datum für das aktuelle Datetime-Object</p>
    METHODS as_date
      RETURNING
        VALUE(pr_date) TYPE dats .
    "! <p class="shorttext synchronized" lang="de">Liefert Datum &amp; Uhrzeit ISO 8601 Zeitstempel</p>
    "!
    "! @parameter pr_iso_timestamp | <p class="shorttext synchronized" lang="de">ISO 8601 Zeitstempel</p>
    METHODS as_iso_timestamp
      RETURNING
        VALUE(pr_iso_timestamp) TYPE string .
    "! <p class="shorttext synchronized" lang="de">Liefert Datum &amp; Uhrzeit als Timestamp</p>
    "!
    "! @parameter pr_timestamp | <p class="shorttext synchronized" lang="de">Datum &amp; Uhrzeit als Timestamp</p>
    METHODS as_long_timestamp
      RETURNING
        VALUE(pr_timestamp) TYPE timestampl .
    "! <p class="shorttext synchronized" lang="de">Liefert Datum &amp; Uhrzeit als Timestamp</p>
    "!
    "! @parameter pr_timestamp | <p class="shorttext synchronized" lang="de">Datum &amp; Uhrzeit als Timestamp</p>
    METHODS as_timestamp
      RETURNING
        VALUE(pr_timestamp) TYPE timestamp .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datum auf Tagesbeginn</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS begin_of_day
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datums auf Monatsbeginn</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS begin_of_month
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datums auf Quartalsbegin</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS begin_of_quarter
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen Datum auf Wochenbeginn</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS begin_of_week
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datums auf Jahresbeginn</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS begin_of_year
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Klont das Datetime-Objekt</p>
    "!
    "! @parameter pr_o_clone | <p class="shorttext synchronized" lang="de">Klon der aktuellen Instanz</p>
    METHODS clone
      RETURNING
        VALUE(pr_o_clone) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzt das Datum auf den Wochentag</p>
    "!
    "! @parameter pi_day_of_week | <p class="shorttext synchronized" lang="de">Wochentag</p>
    "! @parameter pr_o_datetime  | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS day_of_week
      IMPORTING
        !pi_day_of_week      TYPE ty_day_of_week
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Differenz zwischen zwei DateTime-Objekten</p>
    "!
    "! @parameter pi_o_datetime     | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pi_field          | <p class="shorttext synchronized" lang="de">Felder Datum &amp; Uhrzeit</p>
    "! @parameter pi_round_mode     | <p class="shorttext synchronized" lang="de">Rundungsmodus</p>
    "! @parameter pi_round_decimals | <p class="shorttext synchronized" lang="de">Anzahl Stellen Dezimalanteil Rundung</p>
    "! @parameter pr_diff           | <p class="shorttext synchronized" lang="de">Differenz</p>
    METHODS diff
      IMPORTING
        !pi_o_datetime     TYPE REF TO zcl_datetime
        !pi_field          TYPE ty_datetime_field
        !pi_round_mode     TYPE ty_rounding DEFAULT zcl_datetime=>c_rouding-round_nothing
        !pi_round_decimals TYPE i DEFAULT 0
      RETURNING
        VALUE(pr_diff)     TYPE ty_diff .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datum auf Tagesende</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS end_of_day
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datum auf Monatsende</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS end_of_month
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datums auf Quartalsbegin</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS end_of_quarter
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen Datum auf Wochenbeginn</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS end_of_week
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzen des Datums auf Jahresende</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS end_of_year
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Setzt das Datum auf den Montag der ersten Kal.-Woch im Jahr</p>
    "!
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS first_calendar_week
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Liefert das Jahrhundert des Datums</p>
    "!
    "! @parameter pr_century | <p class="shorttext synchronized" lang="de">Das Jahrhundert</p>
    METHODS get_century
      RETURNING
        VALUE(pr_century) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert den Namen des Wochentags</p>
    "!
    "! @parameter pr_dayname | <p class="shorttext synchronized" lang="de">Name des Wochentags</p>
    METHODS get_dayname
      RETURNING
        VALUE(pr_dayname) TYPE string .
    "! <p class="shorttext synchronized" lang="de">Liefert den Tag im Monat</p>
    "!
    "! @parameter pr_day_of_month | <p class="shorttext synchronized" lang="de">Tag im Monat ( 1-31 )</p>
    METHODS get_day_of_month
      RETURNING
        VALUE(pr_day_of_month) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert den Tag in der Woche</p>
    "!
    "! @parameter pr_day_of_week | <p class="shorttext synchronized" lang="de">Tag in der Woche</p>
    METHODS get_day_of_week
      RETURNING
        VALUE(pr_day_of_week) TYPE ty_day_of_week .
    "! <p class="shorttext synchronized" lang="de">Liefert den Tag im Jahr</p>
    "!
    "! @parameter pr_day_of_year | <p class="shorttext synchronized" lang="de">Tag im Jahr (1-365)</p>
    METHODS get_day_of_year
      RETURNING
        VALUE(pr_day_of_year) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert den Wochentag des 1. Januars im Jahr</p>
    "!
    "! @parameter pi_year_offset | <p class="shorttext synchronized" lang="de">Offset Bezugsjahr (+1 nächst. Jahr, -1 letzt. Jahr)</p>
    "! @parameter pr_doom_day    | <p class="shorttext synchronized" lang="de">Wochentag 1. Januars im Jahr</p>
    METHODS get_doom_day
      IMPORTING
        !pi_year_offset    TYPE i DEFAULT 0
      RETURNING
        VALUE(pr_doom_day) TYPE ty_day_of_week .
    "! <p class="shorttext synchronized" lang="de">Liefert den gregoriansichen Tag</p>
    "!
    "! @parameter pr_gregorian_day | <p class="shorttext synchronized" lang="de">Gregoriansicher Tag</p>
    METHODS get_gregorian_day
      RETURNING
        VALUE(pr_gregorian_day) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert die ISO Woche</p>
    "!
    "! @parameter pr_iso_week | <p class="shorttext synchronized" lang="de">Kalenderwoche</p>
    METHODS get_iso_week
      RETURNING
        VALUE(pr_iso_week) TYPE ty_iso_week .
    "! <p class="shorttext synchronized" lang="de">Liefert den Monat des Datums</p>
    METHODS get_month
      RETURNING
        VALUE(pr_month) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert die Anzahl der Tage im Monat</p>
    "!
    "! @parameter pr_number_of_days_of_month | <p class="shorttext synchronized" lang="de">Anzahl der Tage im Monat</p>
    METHODS get_number_of_days_of_month
      RETURNING
        VALUE(pr_number_of_days_of_month) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert die Anzahl der Tage im Jahr</p>
    "!
    "! @parameter pr_number_of_days_of_year | <p class="shorttext synchronized" lang="de">Anzahl der Tage im Jahr</p>
    METHODS get_number_of_days_of_year
      RETURNING
        VALUE(pr_number_of_days_of_year) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert das Quartal des Datum</p>
    "!
    "! @parameter pr_quarter | <p class="shorttext synchronized" lang="de">Das Quartal</p>
    METHODS get_quarter
      RETURNING
        VALUE(pr_quarter) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Liefert das Datum des letzten Tags im Monat</p>
    "!
    "! @parameter pr_last_day_of_month | <p class="shorttext synchronized" lang="de">Letzter Tag im Monat</p>
    METHODS get_ultimo
      RETURNING
        VALUE(pr_last_day_of_month) TYPE dats .
    "! <p class="shorttext synchronized" lang="de">Liefert das Jahr des Datums</p>
    "!
    "! @parameter pr_year | <p class="shorttext synchronized" lang="de">Das Jahr des Datums</p>
    METHODS get_year
      RETURNING
        VALUE(pr_year) TYPE i .
    "! <p class="shorttext synchronized" lang="de">Ist das DateTime-Objekt nach dem anderen Objekt?</p>
    "!
    "! @parameter pi_o_datetime  | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    METHODS is_after
      IMPORTING
        !pi_o_datetime     TYPE REF TO zcl_datetime
        !pi_ignore_time    TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_after) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Ist das DateTime-Objekt nach oder gleich dem anderen Objekt?</p>
    "!
    "! @parameter pi_o_datetime  | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    METHODS is_after_or_equal
      IMPORTING
        !pi_o_datetime              TYPE REF TO zcl_datetime
        !pi_ignore_time             TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_after_or_equal) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Ist das DateTime-Objekt vor dem anderen Objekt?</p>
    "!
    "! @parameter pi_o_datetime  | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    METHODS is_before
      IMPORTING
        !pi_o_datetime      TYPE REF TO zcl_datetime
        !pi_ignore_time     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_before) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Ist das DateTime-Objekt vor oder gleich dem anderen Objekt?</p>
    "!
    "! @parameter pi_o_datetime  | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    METHODS is_before_or_equal
      IMPORTING
        !pi_o_datetime               TYPE REF TO zcl_datetime
        !pi_ignore_time              TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_before_or_equal) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Ist das DateTime-Objekt gleich dem anderen Objekt?</p>
    "!
    "! @parameter pi_o_datetime  | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    "! @parameter pi_ignore_time | <p class="shorttext synchronized" lang="de">Zeitanteil ignorieren?</p>
    METHODS is_equal
      IMPORTING
        !pi_o_datetime      TYPE REF TO zcl_datetime
        !pi_ignore_time     TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(pr_is_before) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Liegt das Datum auf dem letzten eines Monats?</p>
    "!
    "! @parameter pr_fl_is_last_day_of_month | <p class="shorttext synchronized" lang="de">Letzter Tag im Monat?</p>
    METHODS is_last_day_of_month
      RETURNING
        VALUE(pr_fl_is_last_day_of_month) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Ist das Jahr ein Schaltjahr?</p>
    "!
    "! @parameter pr_fl_is_leap | <p class="shorttext synchronized" lang="de">Schaltjahr?</p>
    METHODS is_leap_year
      RETURNING
        VALUE(pr_fl_is_leap) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="de">Setzen eines Felds (Stunde, Tag etc.) im Datum</p>
    "!
    "! @parameter pi_field      | <p class="shorttext synchronized" lang="de">Felder Datum &amp; Uhrzeit</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS set
      IMPORTING
        !pi_field            TYPE ty_datetime_field
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Ändern eines Felds (Stunde, Tag etc.) im Datum</p>
    "!
    "! @parameter pi_field      | <p class="shorttext synchronized" lang="de">Feld im Datum (Stunde, Tag etc.)</p>
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Wert (negative Wert = Addition)</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Instanz Datetime-Objekt für Method Chaining</p>
    METHODS subtract
      IMPORTING
        !pi_field            TYPE ty_datetime_field
        VALUE(pi_value)      TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Tage</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Tage</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_days
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Stunden</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Stunden</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_hours
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Minuten</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Minuten</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_minutes
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Monate</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Wochen</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_months
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Sekunden</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Sekunden</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_seconds
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Wochen</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Wochen</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_weeks
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
    "! <p class="shorttext synchronized" lang="de">Abziehen Jahre</p>
    "!
    "! @parameter pi_value      | <p class="shorttext synchronized" lang="de">Anzahl Jahre</p>
    "! @parameter pr_o_datetime | <p class="shorttext synchronized" lang="de">Datumsfunktionen</p>
    METHODS sub_years
      IMPORTING
        !pi_value            TYPE i
      RETURNING
        VALUE(pr_o_datetime) TYPE REF TO zcl_datetime .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_ts_string(14) TYPE c .
ENDCLASS.



CLASS zcl_datetime IMPLEMENTATION.


  METHOD add.

    DATA: unit TYPE t006-msehi.

    CASE pi_field.
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
            value     = CONV #( pi_field )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    CONVERT DATE me->ca_date TIME me->ca_time
        INTO TIME STAMP DATA(ts) TIME ZONE c_utc.

    CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
      EXPORTING
        timestamp_in    = ts
        timezone        = c_utc
        duration        = pi_value
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
      INTO DATE me->ca_date
           TIME me->ca_time.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD add_days.

    pr_o_datetime = me->add( pi_field = c_fields-day
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD add_hours.

    pr_o_datetime = me->add( pi_field = c_fields-hour
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD add_minutes.

    pr_o_datetime = me->add( pi_field = c_fields-minute
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD add_months.

    pr_o_datetime = me->add( pi_field = c_fields-month
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD add_seconds.

    pr_o_datetime = me->add( pi_field = c_fields-second
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD add_weeks.

    pr_o_datetime = me->add( pi_field = c_fields-week
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD add_years.

    pr_o_datetime = me->add( pi_field = c_fields-year
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD as_date.

    pr_date = me->ca_date.

  ENDMETHOD.


  METHOD as_iso_timestamp.

    CONVERT DATE me->ca_date TIME me->ca_time
     INTO TIME STAMP DATA(ts) TIME ZONE c_utc.

    pr_iso_timestamp = cl_xlf_date_time=>create( ts ).
  ENDMETHOD.


  METHOD as_long_timestamp.

    CONVERT DATE me->ca_date TIME me->ca_time
            INTO TIME STAMP pr_timestamp TIME ZONE c_utc.

  ENDMETHOD.


  METHOD as_timestamp.

    CONVERT DATE me->ca_date TIME me->ca_time
            INTO TIME STAMP pr_timestamp TIME ZONE c_utc.

  ENDMETHOD.


  METHOD begin_of_day.

    me->ca_time = c_times-begin_of_day.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD begin_of_month.

    me->ca_date+6(2) = '01'.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD begin_of_quarter.

    DATA(quarter) = me->get_quarter( ).

    CASE quarter.
      WHEN 1.
        me->ca_date+4(4) = '0101'. ##FIXME.
      WHEN 2.
        me->ca_date+4(4) = '0401'.
      WHEN 3.
        me->ca_date+4(4) = '0701'.
      WHEN 4.
        me->ca_date+4(4) = '1001'.
    ENDCASE.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD begin_of_week.

    DATA(current_dow) = me->get_day_of_week( ).

    me->ca_date = me->ca_date - ( current_dow - 1 ).

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD begin_of_year.

    me->ca_date+4(4) = c_day_of_year-first.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD clone.

    pr_o_clone = NEW #( ).

    pr_o_clone->ca_date = me->ca_date.
    pr_o_clone->ca_time = me->ca_time.

  ENDMETHOD.


  METHOD create_list.

    CASE pi_increment_field.
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
            value     = CONV #( pi_increment_field )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    IF pi_increment_value <= 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_INCREMENT_VALUE'
          value     = CONV #( pi_increment_value )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    IF pi_num_of_elements < 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_NUM_OF_ELEMENTS'
          value     = CONV #( pi_num_of_elements )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    DATA: loop_count TYPE i VALUE 0.

    DATA(o_current) = pi_o_start->clone( ).

    APPEND INITIAL LINE TO pr_t_datetime_list ASSIGNING FIELD-SYMBOL(<wa_datetime>).
    <wa_datetime>-o_datetime = o_current.

    o_current = o_current->clone( ).
    loop_count = loop_count + 1.

    WHILE loop_count < pi_num_of_elements.

      o_current->add( pi_field = pi_increment_field
                      pi_value = pi_increment_value ).

      APPEND INITIAL LINE TO pr_t_datetime_list ASSIGNING <wa_datetime>.
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

    me->add( pi_field = c_fields-day
             pi_value = delta_in_days ).

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD diff.

    min_max( EXPORTING pi_o_datetime_01 = me
                       pi_o_datetime_02 = pi_o_datetime
             IMPORTING pe_o_min_datetime = DATA(o_min)
                       pe_o_max_datetime = DATA(o_max) ).

    DATA(diff) = cl_abap_tstmp=>subtract( tstmp1 = CONV #( o_max->as_timestamp( ) )
                                          tstmp2 = CONV #( o_min->as_timestamp( ) ) ).

    CASE pi_field.
      WHEN c_fields-day.
        pr_diff = CONV #( diff / c_time_constants-num_seconds_per_day ).
      WHEN c_fields-hour.
        pr_diff = CONV #( diff / c_time_constants-num_seconds_per_hour ).
      WHEN c_fields-minute.
        pr_diff = CONV #( diff / c_time_constants-num_seconds_per_minute ).
      WHEN c_fields-week.
        pr_diff = CONV #( diff / c_time_constants-num_seconds_per_week ).
      WHEN c_fields-year.
        pr_diff = CONV #( diff / c_time_constants-num_seconds_per_year ).
      WHEN c_fields-second.
        pr_diff = diff.
      WHEN c_fields-month.

        IF o_min->is_equal( o_max ).
          pr_diff = 0.
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

          pr_diff = CONV #( delta_year_month + ( max_day_offset - min_day_offset ) ).

        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_parameter_invalid_range
          EXPORTING
            parameter = 'PI_FIELD'
            value     = CONV #( pi_field )
            textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDCASE.

    IF pi_round_mode NE c_rouding-round_nothing.

      pr_diff = round( val  = pr_diff
                       dec  = pi_round_decimals
                       mode = pi_round_mode ).

    ENDIF.

  ENDMETHOD.


  METHOD end_of_day.

    me->ca_time = c_times-end_of_day.

    pr_o_datetime = me.

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

    me->set( pi_field = c_fields-day
             pi_value = last_of_month ).

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD end_of_quarter.

    ##TODO. " Repository Github A. Teubner
    ##TODO. " Namensraum prüfen.
    ##TODO. " Abhängigkeiten reduzieren.
    ##TODO. " Eigenes Paket.

    DATA(quarter) = me->get_quarter( ).

    CASE quarter.
      WHEN 1.
        me->ca_date+4(4) = '0331'.
      WHEN 2.
        me->ca_date+4(4) = '0630'.
      WHEN 3.
        me->ca_date+4(4) = '0930'.
      WHEN 4.
        me->ca_date+4(4) = '1231'.
    ENDCASE.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD end_of_week.

    pr_o_datetime = me.

    DATA(current_dow) = me->get_day_of_week( ).

    me->ca_date = me->ca_date + ( c_day_of_week-sunday - current_dow ).

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD end_of_year.

    me->ca_date+4(4) = c_day_of_year-last.

    pr_o_datetime = me.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD first_calendar_week.

    DATA(o_date) = me->clone( )->begin_of_year( ).

    DATA(first_day_of_year) = o_date->get_day_of_week( ).

    " Die erste Kalenderwoche eine Jahres, ist die erste Woche,
    " die mindestens vier Tage des neuen Jahre umfasst. Sprich
    " der Erste eines Jahres muss vor dem Freitag liegen.
    IF first_day_of_year > c_day_of_week-thursday.
      me->ca_date = o_date->begin_of_week( )->as_date( ).
    ELSE.
      me->ca_date = o_date->begin_of_week(
                               )->add_weeks( 1
                               )->as_date( ).
    ENDIF.

    pr_o_datetime = me.

  ENDMETHOD.


  METHOD from_date.

    " sanity check if date is out of bounds.
    " e.g. 29th february of a none leap year.
    IF CONV i( CONV d( pi_date ) ) = 0.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_DATE'
          value     = CONV #( pi_date )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    " sanity check if time is out of bounds.
    IF pi_time IS SUPPLIED AND pi_time NOT BETWEEN c_times-begin_of_day AND c_times-end_of_day.
      RAISE EXCEPTION TYPE cx_parameter_invalid_range
        EXPORTING
          parameter = 'PI_TIME'
          value     = CONV #( pi_time )
          textid    = cx_parameter_invalid_range=>cx_parameter_invalid_range.
    ENDIF.

    pr_o_datetime = NEW #( ).

    pr_o_datetime->ca_date = CONV #( pi_date ).
    pr_o_datetime->ca_time = pi_time.


  ENDMETHOD.


  METHOD from_iso_timestamp.

    DATA(ts) = cl_xlf_date_time=>parse( pi_iso8601 ).

    pr_o_datetime = NEW #( ).

    CONVERT TIME STAMP ts TIME ZONE c_utc
      INTO DATE pr_o_datetime->ca_date
      TIME pr_o_datetime->ca_time.

  ENDMETHOD.


  METHOD from_long_timestamp.

    pr_o_datetime = NEW #( ).

    CONVERT TIME STAMP pi_ts TIME ZONE c_utc
      INTO DATE pr_o_datetime->ca_date
      TIME pr_o_datetime->ca_time.

  ENDMETHOD.


  METHOD from_now.

    " Get current date and time...
    GET TIME STAMP FIELD DATA(ts).

    pr_o_datetime = NEW #( ).

    CONVERT TIME STAMP ts TIME ZONE sy-zonlo
      INTO DATE pr_o_datetime->ca_date
      TIME pr_o_datetime->ca_time.

  ENDMETHOD.


  METHOD from_timestamp.

    pr_o_datetime = NEW #( ).

    CONVERT TIME STAMP pi_ts TIME ZONE c_utc
      INTO DATE pr_o_datetime->ca_date
      TIME pr_o_datetime->ca_time.

  ENDMETHOD.


  METHOD from_today.

    pr_o_datetime = NEW #( ).

    pr_o_datetime->ca_date = sy-datum.
    pr_o_datetime->ca_time = c_times-begin_of_day.

  ENDMETHOD.


  METHOD get_century.

    pr_century = CONV i( ca_date+0(4) ) DIV 100.

  ENDMETHOD.


  METHOD get_dayname.

    DATA: weekday TYPE dtresr-weekday.
    CALL FUNCTION 'DATE_TO_DAY'
      EXPORTING
        date    = me->ca_date
      IMPORTING
        weekday = weekday.

    pr_dayname = weekday.

  ENDMETHOD.


  METHOD get_day_of_month.

    pr_day_of_month = ca_date+6(2).

  ENDMETHOD.


  METHOD get_day_of_week.

    DATA(gd) = me->get_gregorian_day( ).

    DATA(mod_gd) = gd MOD 7.

    CASE mod_gd.
      WHEN 0. pr_day_of_week = c_day_of_week-friday.
      WHEN 1. pr_day_of_week = c_day_of_week-saturday.
      WHEN 2. pr_day_of_week = c_day_of_week-sunday.
      WHEN 3. pr_day_of_week = c_day_of_week-monday.
      WHEN 4. pr_day_of_week = c_day_of_week-tuesday.
      WHEN 5. pr_day_of_week = c_day_of_week-wednesday.
      WHEN 6. pr_day_of_week = c_day_of_week-thursday.
    ENDCASE.


  ENDMETHOD.


  METHOD get_day_of_year.

    DATA(first_day) = me->clone( )->begin_of_year( )->as_date( ).

    pr_day_of_year = me->ca_date - first_day  + 1.

  ENDMETHOD.


  METHOD get_doom_day.

    pr_doom_day = me->clone(
                    )->begin_of_year(
                    )->add_years( pi_year_offset
                    )->get_day_of_week( ).

  ENDMETHOD.


  METHOD get_gregorian_day.

    CONSTANTS: c_first_day_ad TYPE dats VALUE '00010101'.

    pr_gregorian_day = me->ca_date - c_first_day_ad + 1.

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

    pr_iso_week-week        = base_week.
    pr_iso_week-year        = base_year.
    pr_iso_week-day_of_week = me->get_day_of_week( ).

  ENDMETHOD.


  METHOD get_month.

    pr_month = ca_date+4(2).

  ENDMETHOD.


  METHOD get_number_of_days_of_month.

    DATA(ultimo) = me->get_ultimo( ).
    pr_number_of_days_of_month =  ultimo+6(2).

  ENDMETHOD.


  METHOD get_number_of_days_of_year.

    IF me->is_leap_year( ) = abap_true.
      pr_number_of_days_of_year =  c_day_of_year-max_days_in_leap_year.
    ELSE.
      pr_number_of_days_of_year =  c_day_of_year-max_days_in_none_leap_year.
    ENDIF.

  ENDMETHOD.


  METHOD get_quarter.

    DATA(month) = me->get_month( ).

    DATA(quarter) = CONV f( month / 3 ).

    pr_quarter = round( val = quarter dec = 0 mode = cl_abap_math=>round_ceiling ) .

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
        pr_last_day_of_month    = me->ca_date+0(6) && 31.
      WHEN c_month-february.
        IF me->is_leap_year( ) = abap_true.
          pr_last_day_of_month  = me->ca_date+0(6) && 29.
        ELSE.
          pr_last_day_of_month  = me->ca_date+0(6) && 28.
        ENDIF.
      WHEN OTHERS.
        pr_last_day_of_month    = me->ca_date+0(6) && 30.
    ENDCASE.

  ENDMETHOD.


  METHOD get_year.

    pr_year = ca_date+0(4).

  ENDMETHOD.


  METHOD is_after.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( pi_o_datetime->as_timestamp( ) ).

    " Transient time...
    IF pi_ignore_time = abap_true.
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
    DATA(other_ts) = CONV ty_ts_string( pi_o_datetime->as_timestamp( ) ).

    " Transient time...
    IF pi_ignore_time = abap_true.
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
    DATA(other_ts) = CONV ty_ts_string( pi_o_datetime->as_timestamp( ) ).

    " Transient time...
    IF pi_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts < other_ts.
      pr_is_before =  abap_true.
    ELSE.
      pr_is_before =  abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD is_before_or_equal.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( pi_o_datetime->as_timestamp( ) ).

    " Transient time...
    IF pi_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts <= other_ts.
      pr_is_before_or_equal =  abap_true.
    ELSE.
      pr_is_before_or_equal =  abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_equal.

    DATA(this_ts)  = CONV ty_ts_string( me->as_timestamp( ) ).
    DATA(other_ts) = CONV ty_ts_string( pi_o_datetime->as_timestamp( ) ).

    " Transient time...
    IF pi_ignore_time = abap_true.
      this_ts+8(6)  = c_times-begin_of_day.
      other_ts+8(6) = c_times-begin_of_day.
    ENDIF.

    IF this_ts = other_ts.
      pr_is_before =  abap_true.
    ELSE.
      pr_is_before =  abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD is_last_day_of_month.

    DATA(last_day_of_month) = me->get_ultimo( ).

    pr_fl_is_last_day_of_month = COND #( WHEN me->ca_date = last_day_of_month
                                            THEN  abap_true
                                            ELSE  abap_false ).

  ENDMETHOD.


  METHOD is_leap_year.

    DATA(year) = me->get_year( ).

    DATA(mod_4)   = year MOD 4.
    DATA(mod_100) = year MOD 100.
    DATA(mod_400) = year MOD 400.

    IF mod_4 = 0 AND mod_100 NE 0.
      pr_fl_is_leap = abap_true.
    ELSEIF mod_400 = 0.
      pr_fl_is_leap = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD max.

    IF pi_o_datetime_01->is_equal( pi_o_datetime = pi_o_datetime_02 pi_ignore_time = pi_ignore_time ).
      pr_o_max = pi_o_datetime_01.
      RETURN.
    ENDIF.

    IF pi_o_datetime_01->is_before( pi_o_datetime = pi_o_datetime_02 pi_ignore_time = pi_ignore_time ).
      pr_o_max = pi_o_datetime_01.
      RETURN.
    ELSE.
      pr_o_max = pi_o_datetime_02.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD min.

    IF pi_o_datetime_01->is_equal( pi_o_datetime = pi_o_datetime_02 pi_ignore_time = pi_ignore_time ).
      pr_o_min = pi_o_datetime_02.
      RETURN.
    ENDIF.

    IF pi_o_datetime_01->is_before( pi_o_datetime = pi_o_datetime_02 pi_ignore_time = pi_ignore_time ).
      pr_o_min = pi_o_datetime_02.
      RETURN.
    ELSE.
      pr_o_min = pi_o_datetime_01.
      RETURN.
    ENDIF.



  ENDMETHOD.


  METHOD min_max.

    IF pi_o_datetime_01->is_equal( pi_o_datetime = pi_o_datetime_02 pi_ignore_time = pi_ignore_time ).
      pe_o_min_datetime = pi_o_datetime_01.
      pe_o_max_datetime = pi_o_datetime_02.
      RETURN.
    ENDIF.

    IF pi_o_datetime_01->is_before( pi_o_datetime = pi_o_datetime_02 pi_ignore_time = pi_ignore_time ).
      pe_o_min_datetime = pi_o_datetime_01.
      pe_o_max_datetime = pi_o_datetime_02.
      RETURN.
    ENDIF.

    pe_o_min_datetime = pi_o_datetime_02.
    pe_o_max_datetime = pi_o_datetime_01.

  ENDMETHOD.


  METHOD set.

    pr_o_datetime = me.

    CASE pi_field.
      WHEN c_fields-second.
        IF pi_value < 0 OR pi_value > 59.
          RETURN.
        ENDIF.
        me->ca_time+4(2) = |{ pi_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-minute.
        IF pi_value < 0 OR pi_value > 60.
          RETURN.
        ENDIF.
        me->ca_time+2(2) = |{ pi_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-hour.
        IF pi_value < 0 OR pi_value > 23.
          RETURN.
        ENDIF.
        me->ca_time+0(2) = |{ pi_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-day.
        DATA(number_of_days_in_month) = me->get_number_of_days_of_month( ).
        IF pi_value < 0 OR pi_value > number_of_days_in_month.
          RETURN.
        ENDIF.
        me->ca_date+6(2) = |{ pi_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-month.
        IF pi_value < 1 OR pi_value > 12.
          RETURN.
        ENDIF.
        me->ca_date+4(2) = |{ pi_value WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-year.
        IF pi_value < 0 OR pi_value > 9999.
          RETURN.
        ENDIF.
        me->ca_date+0(4) = |{ pi_value WIDTH = 4 PAD = '0' ALIGN = RIGHT }|.
      WHEN c_fields-week.
        IF pi_value < 1 OR pi_value > 53.
          RETURN.
        ENDIF.
        DATA(new_date) = me->clone(
                                )->begin_of_year(
                                )->add_weeks( pi_value
                                )->begin_of_week(
                                )->add_days( me->get_day_of_week( ) - 1
                                )->as_date( ).
        me->ca_date = new_date.
    ENDCASE.

  ENDMETHOD.


  METHOD subtract.

    pi_value = pi_value * -1.

    pr_o_datetime = me->add( pi_field = pi_field
                             pi_value = pi_value ).

  ENDMETHOD.


  METHOD sub_days.

    pr_o_datetime = me->add( pi_field = c_fields-day
                             pi_value = pi_value * -1 ).

  ENDMETHOD.


  METHOD sub_hours.

    pr_o_datetime = me->add( pi_field = c_fields-hour
                             pi_value = pi_value * -1 ).

  ENDMETHOD.


  METHOD sub_minutes.

    pr_o_datetime = me->add( pi_field = c_fields-minute
                             pi_value = pi_value * -1 ).

  ENDMETHOD.


  METHOD sub_months.

    pr_o_datetime = me->add( pi_field = c_fields-month
                             pi_value = pi_value * -1 ).

  ENDMETHOD.


  METHOD sub_seconds.

    pr_o_datetime = me->add( pi_field = c_fields-second
                             pi_value = pi_value * -1 ).

  ENDMETHOD.


  METHOD sub_weeks.

    pr_o_datetime = me->add( pi_field = c_fields-week
                             pi_value = pi_value * -1 ).

  ENDMETHOD.


  METHOD sub_years.

    pr_o_datetime = me->add( pi_field = c_fields-year
                             pi_value = pi_value * -1 ).

  ENDMETHOD.
ENDCLASS.
