
CLASS unit_tests DEFINITION FOR TESTING
  FINAL
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>unit_Tests
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZZZCL_DATETIME
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.

    METHODS: method_chaining FOR TESTING.
    METHODS: add FOR TESTING.
    METHODS: add_days FOR TESTING.
    METHODS: add_hours FOR TESTING.
    METHODS: add_minutes FOR TESTING.
    METHODS: add_months FOR TESTING.
    METHODS: add_seconds FOR TESTING.
    METHODS: add_weeks FOR TESTING.
    METHODS: add_years FOR TESTING.
    METHODS: as_date FOR TESTING.
    METHODS: as_iso_timestamp FOR TESTING.
    METHODS: as_long_timestamp FOR TESTING.
    METHODS: as_timestamp FOR TESTING.
    METHODS: begin_of_day FOR TESTING.
    METHODS: begin_of_month FOR TESTING.
    METHODS: begin_of_quarter FOR TESTING.
    METHODS: begin_of_week FOR TESTING.
    METHODS: begin_of_year FOR TESTING.
    METHODS: clone FOR TESTING.
    METHODS: day_of_week FOR TESTING.
    METHODS: end_of_day FOR TESTING.
    METHODS: end_of_month FOR TESTING.
    METHODS: end_of_quarter FOR TESTING.
    METHODS: end_of_week FOR TESTING.
    METHODS: end_of_year FOR TESTING.
    METHODS: from_date FOR TESTING.
    METHODS: from_iso_timestamp FOR TESTING.
    METHODS: from_now FOR TESTING.
    METHODS: from_today FOR TESTING.
    METHODS: get_dayname FOR TESTING.
    METHODS: get_day_of_week FOR TESTING.
    METHODS: get_day_of_year FOR TESTING.
    METHODS: get_ultimo FOR TESTING.
    METHODS: get_number_of_days_of_month FOR TESTING.
    METHODS: get_number_of_days_of_year FOR TESTING.
    METHODS: is_last_day_of_month FOR TESTING.
    METHODS: set FOR TESTING.
    METHODS: subtract FOR TESTING.
    METHODS: get_quarter FOR TESTING.
    METHODS: get_iso_week FOR TESTING.
    METHODS: is_leap_year FOR TESTING.
    METHODS: is_before FOR TESTING.
    METHODS: is_before_or_equal FOR TESTING.
    METHODS: is_after FOR TESTING.
    METHODS: is_after_or_equal FOR TESTING.
    METHODS: is_equal FOR TESTING.
    METHODS: create_list FOR TESTING.
    METHODS: sub_days FOR TESTING.
    METHODS: sub_hours FOR TESTING.
    METHODS: sub_minutes FOR TESTING.
    METHODS: sub_months FOR TESTING.
    METHODS: sub_seconds FOR TESTING.
    METHODS: sub_weeks FOR TESTING.
    METHODS: sub_years FOR TESTING.
    METHODS: diff FOR TESTING.

ENDCLASS.       "unit_Tests


CLASS unit_tests IMPLEMENTATION.

  METHOD method_chaining.
    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ).

    o_datetime->add_days( 3
      )->add_hours( 12
      )->add_minutes( 30
      )->add_seconds( 45
      )->add_weeks( 2
      )->add_months( 3
      )->add_years( 3 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20240624123045'
                                    msg = 'Method chaining check #01' ).


    o_datetime->add_days( -3
       )->add_hours( -12
       )->add_minutes( -30
       )->add_seconds( -45
       )->add_weeks( -2
       )->add_months( -3
       )->add_years( -3 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                   exp = '20210307000000'
                                   msg = 'Method chaining check #02' ).

  ENDMETHOD.

  METHOD add.
    " ISO Timestamp im Format 2021-01-01T01:01:01+01:00
    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ).


    o_datetime->add( iv_field = zcl_datetime=>c_fields-day
                     iv_value = 3 ).


    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210310000000'
                                    msg = 'Datum +3 Tage fehlerhaft' ).

    o_datetime->add( iv_field = zcl_datetime=>c_fields-day
                     iv_value = -5 ).

    o_datetime->add( iv_field = zcl_datetime=>c_fields-hour
                     iv_value = -3 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210304210000'
                                    msg = 'Datum -5 Tage - 3 Stunden fehlerhaft' ).

    o_datetime->add( iv_field = zcl_datetime=>c_fields-week
                     iv_value = 1 ).

    o_datetime->add( iv_field = zcl_datetime=>c_fields-hour
                     iv_value = 3 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                        exp = '20210312000000'
                                        msg = 'Datum +1 Woche +3 Stunden fehlerhaft' ).

    o_datetime = zcl_datetime=>from_date( iv_date = '20210228' iv_time = zcl_datetime=>c_times-end_of_day ).

    o_datetime->add( iv_field = zcl_datetime=>c_fields-second
                     iv_value = 1 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210301000000'
                                    msg = 'Datum +1 Sekunde fehlerhaft' ).

  ENDMETHOD.


  METHOD add_days.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ).

    o_datetime->add_days( 5 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210312000000'
                                    msg = 'Datum +5 Tage fehlerhaft' ).

    o_datetime->add_days( -12 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210228000000'
                                    msg = 'Datum -12 Tage fehlerhaft' ).

  ENDMETHOD.


  METHOD add_hours.


    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307'  iv_time = '060000' ).

    o_datetime->add_hours( 8 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307140000'
                                    msg = 'Add hours check #01' ).

    o_datetime->add_hours( 16 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210308060000'
                                    msg = 'Add hours check #02' ).

    o_datetime->add_hours( -24 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307060000'
                                    msg = 'Add hours check #03' ).


  ENDMETHOD.


  METHOD add_minutes.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307'  iv_time = '060000' ).

    o_datetime->add_minutes( 8 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307060800'
                                    msg = 'Add minutes check #01' ).

    o_datetime->add_minutes( 52 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307070000'
                                    msg = 'Add minutes check #02' ).

    o_datetime->add_minutes( 60 * 24 * 2 ).


    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210309070000'
                                    msg = 'Add minutes check #03' ).

    o_datetime->add_minutes( -1 * ( 60 * 24 * 3 ) ).
    o_datetime->add_minutes( -60 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210306060000'
                                    msg = 'Add minutes check #04' ).

  ENDMETHOD.


  METHOD add_months.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210131' ).

    DATA(date) = o_datetime->add_months( 1 )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210228'
                                    msg = 'Add month check #01' ).

    date = o_datetime->add_months( 36 )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20240228'
                                    msg = 'Add month check #02' ).


  ENDMETHOD.


  METHOD add_seconds.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307'  iv_time = '060000' ).

    o_datetime->add_seconds( 8 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307060008'
                                    msg = 'Add seconds check #01' ).

    o_datetime->add_seconds( 52 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307060100'
                                    msg = 'Add seconds check #02' ).

    o_datetime->add_seconds( 60 * 60 * 24 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210308060100'
                                    msg = 'Add seconds check #03' ).

    o_datetime->add_seconds( -600 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210308055100'
                                    msg = 'Add seconds check #04' ).

  ENDMETHOD.


  METHOD add_weeks.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' ).

    o_datetime->add_weeks( 1 ).

    DATA(date) = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210314'
                                    msg = 'Add weeks check #01' ).

    o_datetime->add_weeks( -2 ).

    date = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210228'
                                    msg = 'Add weeks check #02' ).
  ENDMETHOD.


  METHOD add_years.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' ).

    o_datetime->add_years( 1 ).

    DATA(date) = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20220307'
                                    msg = 'Add years check #01' ).

    o_datetime->add_years( -2 ).

    date = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20200307'
                                    msg = 'Add years check #02' ).

  ENDMETHOD.


  METHOD as_date.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' ).

    DATA(date) = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                 exp = '20210307'
                                 msg = 'As date check #01' ).

    o_datetime = zcl_datetime=>from_date( iv_date = '20210331' ).

    date = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                 exp = '20210331'
                                 msg = 'As date check #02' ).

    o_datetime = zcl_datetime=>from_date( iv_date = '20210401' ).

    date = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                 exp = '20210401'
                                 msg = 'As date check #02' ).

  ENDMETHOD.


  METHOD as_iso_timestamp.

    DATA(o_datetime) = zcl_datetime=>from_timestamp( '20210307093045' ).

    DATA(iso_timestamp) = o_datetime->as_iso_timestamp( ).

    cl_aunit_assert=>assert_equals( act = iso_timestamp
                                    exp = '2021-03-07T09:30:45Z'
                                    msg = 'As ISO timestamp check #01' ).
  ENDMETHOD.


  METHOD as_long_timestamp.

    DATA(o_datetime) = zcl_datetime=>from_timestamp( '20210307093045' ).

    DATA(long_timestamp) = o_datetime->as_long_timestamp( ).

    cl_aunit_assert=>assert_equals( act = long_timestamp
                                    exp = '20210307093045.0000000'
                                    msg = 'As date check #01' ).

  ENDMETHOD.


  METHOD as_timestamp.

    DATA(o_datetime) = zcl_datetime=>from_timestamp( '20210307093045' ).

    DATA(long_timestamp) = o_datetime->as_long_timestamp( ).

    cl_aunit_assert=>assert_equals( act = long_timestamp
                                    exp = '20210307093045'
                                    msg = 'As date check #02' ).

  ENDMETHOD.


  METHOD begin_of_day.


    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' iv_time = '235959' ).

    o_datetime->begin_of_day( ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307000000'
                                    msg = 'Begin of day check #01' ).
  ENDMETHOD.


  METHOD begin_of_month.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307'  ).

    o_datetime->begin_of_month( ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210301000000'
                                    msg = 'Begin of month check #01' ).

  ENDMETHOD.


  METHOD begin_of_quarter.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ).

    o_datetime->begin_of_quarter( ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210101000000'
                                    msg = 'Begin of quarter check #01' ).

  ENDMETHOD.


  METHOD begin_of_week.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).
    DATA(date) = o_datetime->begin_of_week( )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20201228'
                                    msg = 'Begin of week check #01' ).

  ENDMETHOD.


  METHOD begin_of_year.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ).

    o_datetime->begin_of_quarter( ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210101000000'
                                    msg = 'Begin of year check #01' ).

  ENDMETHOD.


  METHOD clone.

    DATA(o_this_datetime) = zcl_datetime=>from_now( ).

    DATA(o_other_datetime) = o_this_datetime->clone( ).

    IF o_this_datetime = o_other_datetime.
      cl_aunit_assert=>fail( 'Clone check #01' ).
    ENDIF.

    IF NOT o_this_datetime->is_equal( o_other_datetime ).
      cl_aunit_assert=>fail( 'Clone check #02' ).
    ENDIF.

  ENDMETHOD.


  METHOD day_of_week.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ). " Sunday

    o_datetime->day_of_week( zcl_datetime=>c_day_of_week-wednesday ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210303000000'
                                    msg = 'Day of week check #01' ).

    o_datetime->day_of_week( zcl_datetime=>c_day_of_week-monday ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210301000000'
                                    msg = 'Day of week check #02' ).

  ENDMETHOD.


  METHOD end_of_day.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' iv_time = '120000' ).

    o_datetime->end_of_day( ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307235959'
                                    msg = 'End of day check #01' ).

  ENDMETHOD.


  METHOD end_of_month.

    DATA(o_datetime) = zcl_datetime=>from_date( '20200205' ).

    DATA(ts) = o_datetime->end_of_month( )->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                   exp = '20200229000000'
                                   msg = 'End of month check #01' ).

    o_datetime = zcl_datetime=>from_date( '20210205' ).

    ts = o_datetime->end_of_month( )->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210228000000'
                                    msg = 'End of month check #02' ).

  ENDMETHOD.


  METHOD end_of_quarter.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ). " Sunday

    o_datetime->end_of_quarter( ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210331000000'
                                    msg = 'End of quarter check #01' ).

    o_datetime->add_days( 1  )->end_of_quarter( ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210630000000'
                                    msg = 'End of quarter check #02' ).

    o_datetime->add_days( 1  )->end_of_quarter( ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210930000000'
                                    msg = 'End of quarter check #03' ).

    o_datetime->add_days( 1  )->end_of_quarter( ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20211231000000'
                                    msg = 'End of quarter check #04' ).

  ENDMETHOD.


  METHOD end_of_week.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).
    DATA(date) = o_datetime->end_of_week( )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210103'
                                    msg = 'End of week check #01' ).

    o_datetime = zcl_datetime=>from_date( '20210301' ).
    date = o_datetime->end_of_week( )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210307'
                                    msg = 'End of week check #02' ).

  ENDMETHOD.


  METHOD end_of_year.
    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    DATA(date) = o_datetime->end_of_year( )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20211231'
                                    msg = 'End of year check #01' ).
  ENDMETHOD.


  METHOD from_date.
    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    DATA(date) = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210101'
                                    msg = 'From date check #01' ).

    TRY.
        o_datetime = zcl_datetime=>from_date( '20210229' ). " 2021 is a none leap year...29th of february is not a valid date.
        cl_aunit_assert=>fail( msg = 'From date check #02' ).
      CATCH cx_parameter_invalid_range ##NO_HANDLER.
        " Everything is fine. We had expected an exception.
    ENDTRY.

    TRY.
        o_datetime = zcl_datetime=>from_date( '20200229' ). " 2021 is a leap year...29th of february is a valid date.
      CATCH cx_parameter_invalid_range ##NO_HANDLER.
        cl_aunit_assert=>fail( msg = 'From date check #03' ).
        " Everything is fine. We had expected an exception.
    ENDTRY.

    TRY.
        o_datetime = zcl_datetime=>from_date( iv_date = '20200229' iv_time = '243030' ).
        cl_aunit_assert=>fail( msg = 'From date check #04' ).
      CATCH cx_parameter_invalid_range ##NO_HANDLER.
        " Everything is fine. We had expected an exception.
    ENDTRY.

    TRY.
        o_datetime = zcl_datetime=>from_date( iv_date = '20200229' iv_time = '000000' ).
      CATCH cx_parameter_invalid_range ##NO_HANDLER.
        cl_aunit_assert=>fail( msg = 'From date check #05' ).
    ENDTRY.

  ENDMETHOD.


  METHOD from_iso_timestamp.

    DATA(o_datetime) = zcl_datetime=>from_iso_timestamp( '2020-04-21T18:09:25Z' ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20200421180925'
                                    msg = 'From iso timestamp check #01' ).

    TRY.
        o_datetime = zcl_datetime=>from_iso_timestamp( '2020-04-21T28:09:25Z' ). " 28:09:25 is not a valid time...
        cl_aunit_assert=>fail( msg = 'From iso timestamp check #02' ).
      CATCH cx_xlf_illegal_argument ##NO_HANDLER.
        " Everything is fine. We had expected an exception.
    ENDTRY.


  ENDMETHOD.


  METHOD from_now.

    DATA(current_date) = sy-datum.
    DATA(current_time) = sy-uzeit.

    DATA(o_datetime) = zcl_datetime=>from_now( ).
    DATA(ts) = o_datetime->as_timestamp( ).

    DATA(o_dt_reference) = zcl_datetime=>from_timestamp(  CONV timestamp( |{ current_date }{ current_time }| ) ).

    DATA(delta) = abs( o_datetime->diff( iv_field      = zcl_datetime=>c_fields-second
                                         io_datetime = o_dt_reference ) ).

    " Max delta of one second is okay. Be aware that this test fails in debugger
    " if you arent fast enough...
    IF delta > 1.
      cl_aunit_assert=>fail( msg = 'From now check #01' ).
    ENDIF.

  ENDMETHOD.


  METHOD from_today.

    DATA(current_date) = sy-datum.
    DATA(o_datetime) = zcl_datetime=>from_today( ).

    DATA(date) = o_datetime->as_date( ).

    IF date NE current_date.
      cl_aunit_assert=>fail( msg = 'From today check #01' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_dayname.

    ##TODO.
  ENDMETHOD.


  METHOD get_day_of_week.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    " We step throug next 7 day and check the expected day of week.
    " Checks are done relative to given date 1st January of 2021
    DATA(day_of_week) = o_datetime->get_day_of_week( ).

    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-friday
                                    msg = 'Day of week check #01' ).

    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-saturday
                                    msg = 'Day of week check #02' ).

    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-sunday
                                    msg = 'Day of week check #03' ).

    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-monday
                                    msg = 'Day of week check #04' ).

    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-tuesday
                                    msg = 'Day of week check #05' ).

    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-wednesday
                                    msg = 'Day of week check #06' ).
    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-thursday
                                    msg = 'Day of week check #07' ).

    o_datetime->add_days( 1 ).
    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-friday
                                    msg = 'Day of week check #08' ).

    o_datetime = zcl_datetime=>from_date( '20211231' ).

    day_of_week = o_datetime->get_day_of_week( ).
    cl_aunit_assert=>assert_equals( act = day_of_week
                                    exp = zcl_datetime=>c_day_of_week-friday
                                    msg = 'Day of week check #09' ).

  ENDMETHOD.


  METHOD get_ultimo.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    DATA(ultimo) = o_datetime->get_ultimo( ).

    cl_aunit_assert=>assert_equals( act = ultimo
                                    exp = '20210131'
                                    msg = 'Get last day of month check #01' ).

    ultimo = o_datetime->add_months( 1
                            )->get_ultimo( ).

    cl_aunit_assert=>assert_equals( act = ultimo
                                    exp = '20210228'
                                    msg = 'Get last day of month check #02' ).

    ultimo = o_datetime->add_years( -1
                           )->get_ultimo( ). " 2020 was a leap year...

    cl_aunit_assert=>assert_equals( act = ultimo
                                    exp = '20200229'
                                    msg = 'Get last day of month check #03' ).

  ENDMETHOD.


  METHOD get_number_of_days_of_month.

    " The result is calculated in the same way.
    me->get_ultimo( ).

  ENDMETHOD.


  METHOD get_number_of_days_of_year.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    DATA(number_of_days_of_year) = o_datetime->get_number_of_days_of_year( ).

    cl_aunit_assert=>assert_equals( act = number_of_days_of_year
                                    exp = zcl_datetime=>c_day_of_year-max_days_in_none_leap_year
                                    msg = 'Get number of days of year check #01' ).

    number_of_days_of_year = o_datetime->add_years( -1
                                )->get_number_of_days_of_year( ).

    cl_aunit_assert=>assert_equals( act = number_of_days_of_year
                                    exp = zcl_datetime=>c_day_of_year-max_days_in_leap_year
                                    msg = 'Get number of days of year check #02' ).

  ENDMETHOD.


  METHOD is_last_day_of_month.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210131' ).

    DATA(fl_last_day_of_month) = o_datetime->is_last_day_of_month( ).

    cl_aunit_assert=>assert_equals( act = fl_last_day_of_month
                                    exp = abap_true
                                    msg = 'Is last day of month check #01' ).

    fl_last_day_of_month = o_datetime->add_days( 1
                                )->is_last_day_of_month( ).

    cl_aunit_assert=>assert_equals( act = fl_last_day_of_month
                                    exp = abap_false
                                    msg = 'Is last day of month check #02' ).


  ENDMETHOD.


  METHOD set.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210131' ).

    o_datetime->set( iv_field = zcl_datetime=>c_fields-year
                     iv_value = 2020 ).
    o_datetime->set( iv_field = zcl_datetime=>c_fields-month
                     iv_value = 3 ).
    o_datetime->set( iv_field = zcl_datetime=>c_fields-day
                    iv_value = 1 ).

    o_datetime->set( iv_field = zcl_datetime=>c_fields-hour
                   iv_value = 9 ).
    o_datetime->set( iv_field = zcl_datetime=>c_fields-minute
                    iv_value = 30 ).
    o_datetime->set( iv_field = zcl_datetime=>c_fields-second
                    iv_value = 45 ).


    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20200301093045'
                                    msg = 'Set check #01' ).

  ENDMETHOD.


  METHOD subtract.

  ENDMETHOD.


  METHOD get_day_of_year.

    DATA(o_datetime) = zcl_datetime=>from_date( '19900806' ).

    o_datetime->get_day_of_year( ).
  ENDMETHOD.

  METHOD get_quarter.
    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    DATA(quarter) = o_datetime->get_quarter( ).

    cl_aunit_assert=>assert_equals( act = quarter
                                    exp = 1
                                    msg = 'Quarter calc check #01' ).

    o_datetime->add_months( 3 ).

    quarter = o_datetime->get_quarter( ).

    cl_aunit_assert=>assert_equals( act = quarter
                                    exp = 2
                                    msg = 'Quarter calc check #02' ).

    o_datetime->add_months( 3 ).

    quarter = o_datetime->get_quarter( ).

    cl_aunit_assert=>assert_equals( act = quarter
                                    exp = 3
                                    msg = 'Quarter calc check #03' ).

    o_datetime->add_months( 3 ).

    quarter = o_datetime->get_quarter( ).

    cl_aunit_assert=>assert_equals( act = quarter
                                    exp = 4
                                    msg = 'Quarter calc check #04' ).

    o_datetime->add_months( 3 ).

    quarter = o_datetime->get_quarter( ).

    cl_aunit_assert=>assert_equals( act = quarter
                                    exp = 1
                                    msg = 'Quarter calc check #05' ).

  ENDMETHOD.

  METHOD get_iso_week.

    DATA(o_datetime) = zcl_datetime=>from_date( '20211231' ).

    DATA(wa_iso_week) = o_datetime->get_iso_week( ).

    cl_aunit_assert=>assert_equals( act         = wa_iso_week
                                    exp         = VALUE zcl_datetime=>ty_iso_week(
                                    week        = 52
                                    year        = 2021
                                    day_of_week = zcl_datetime=>c_day_of_week-friday )
                                    msg = 'ISO week check #01' ).

    o_datetime = zcl_datetime=>from_date( '20210101' ).

    wa_iso_week = o_datetime->get_iso_week( ).

    cl_aunit_assert=>assert_equals( act = wa_iso_week
                                    exp = VALUE zcl_datetime=>ty_iso_week(
                                        week        = 53
                                        year        = 2020
                                        day_of_week = zcl_datetime=>c_day_of_week-friday )
                                    msg = 'ISO week check #02' ).

    o_datetime = zcl_datetime=>from_date( '20200104' ).

    wa_iso_week = o_datetime->get_iso_week( ).

    cl_aunit_assert=>assert_equals( act = wa_iso_week
                                    exp = VALUE zcl_datetime=>ty_iso_week(
                                        week        = 1
                                        year        = 2020
                                        day_of_week = zcl_datetime=>c_day_of_week-saturday )
                                    msg = 'ISO week check #03' ).

    o_datetime = zcl_datetime=>from_date( '20200229' ).

    wa_iso_week = o_datetime->get_iso_week( ).

    cl_aunit_assert=>assert_equals( act = wa_iso_week
                                    exp = VALUE zcl_datetime=>ty_iso_week(
                                            week        = 9
                                            year        = 2020
                                            day_of_week = zcl_datetime=>c_day_of_week-saturday )
                                    msg = 'ISO week check #04' ).

    o_datetime = zcl_datetime=>from_date( '20200106' ).

    wa_iso_week = o_datetime->get_iso_week( ).

    cl_aunit_assert=>assert_equals( act = wa_iso_week
                                    exp = VALUE zcl_datetime=>ty_iso_week(
                                            week        = 2
                                            year        = 2020
                                            day_of_week = zcl_datetime=>c_day_of_week-monday )
                                    msg = 'ISO week check #05' ).

  ENDMETHOD.

  METHOD is_leap_year.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210101' ).

    DATA(fl_is_leap_year) = o_datetime->is_leap_year( ).


    cl_aunit_assert=>assert_equals( act = fl_is_leap_year
                                    exp = abap_false
                                    msg = 'Leap year check #01' ).

    o_datetime = zcl_datetime=>from_date( '20200101' ).
    fl_is_leap_year = o_datetime->is_leap_year( ).

    cl_aunit_assert=>assert_equals( act = fl_is_leap_year
                                    exp = abap_true
                                    msg = 'Leap year check #02' ).

    o_datetime = zcl_datetime=>from_date( '16000101' ).

    fl_is_leap_year = o_datetime->is_leap_year( ).

    cl_aunit_assert=>assert_equals( act = fl_is_leap_year
                                    exp = abap_true
                                    msg = 'Leap year check #03' ).

    o_datetime = zcl_datetime=>from_date( '19000101' ).

    fl_is_leap_year = o_datetime->is_leap_year( ).

    cl_aunit_assert=>assert_equals( act = fl_is_leap_year
                                    exp = abap_false
                                    msg = 'Leap year check #04' ).

  ENDMETHOD.

  METHOD is_before.

    DATA(o_this)  = zcl_datetime=>from_now( ).
    DATA(o_other) = o_this->clone( )->add_days( -1 ).

    IF o_other->is_before( o_this ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is before check #01' ).
    ENDIF.

    IF o_this->is_before( o_other ).
      cl_aunit_assert=>fail( msg = 'Is before check #02' ).
    ELSE.
      " Works as expected...
    ENDIF.

    o_other = o_this->clone( )->add_seconds( -1 ).

    IF o_other->is_before( o_this ).
      cl_aunit_assert=>fail( msg = 'Is before check #03' ).
    ELSE.
      " Works as expected...
    ENDIF.

    IF o_this->is_before( o_other ).
      cl_aunit_assert=>fail( msg = 'Is before check #04' ).
    ELSE.
      " Works as expected...
    ENDIF.

    IF o_other->is_before( io_datetime = o_this iv_ignore_time = abap_false ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is before check #05' ).
    ENDIF.


  ENDMETHOD.

  METHOD is_before_or_equal.

    DATA(o_this)  = zcl_datetime=>from_now( ).
    DATA(o_other) = o_this->clone( ).

    IF o_other->is_before_or_equal( o_this ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is before or equal check #01' ).
    ENDIF.

    o_other->add_seconds( -1 ).

    IF o_other->is_before_or_equal( o_this ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is before or equal check #02' ).
    ENDIF.

    o_other = o_this->clone( )->add_seconds( 1 ).

    IF o_other->is_before_or_equal( o_this ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is before or equal check #03' ).
    ENDIF.

    IF o_other->is_before_or_equal( io_datetime = o_this iV_ignore_time = abap_false ).
      cl_aunit_assert=>fail( msg = 'Is before or equal check #03' ).
    ELSE.
      " Works as expected...
    ENDIF.

  ENDMETHOD.

  METHOD is_after.

    DATA(o_this)  = zcl_datetime=>from_now( ).
    DATA(o_other) = o_this->clone( ).

    IF o_this->is_after( o_other ).
      cl_aunit_assert=>fail( msg = 'Is after check #01' ).
    ELSE.
      " Works as expected...
    ENDIF.

    o_other->add_days( - 1 ).

    IF o_this->is_after( o_other ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is after check #02' ).
    ENDIF.

    o_other = o_this->clone( )->add_seconds( -1 ).

    IF o_this->is_after( io_datetime = o_other iv_ignore_time = abap_false ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is after check #03' ).
    ENDIF.

    o_other = o_this->clone( )->add_seconds( 1 ).

    IF o_this->is_after( io_datetime = o_other iv_ignore_time = abap_false ).
      cl_aunit_assert=>fail( msg = 'Is after check #03' ).
    ELSE.
      " Works as expected...
    ENDIF.

  ENDMETHOD.

  METHOD is_after_or_equal.

    DATA(o_this)  = zcl_datetime=>from_now( ).
    DATA(o_other) = o_this->clone( ).

    IF o_this->is_after_or_equal( o_other ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is after or equal check #01' ).
    ENDIF.

    o_other->add_days( 1 ).

    IF o_this->is_after_or_equal( o_other ).
      cl_aunit_assert=>fail( msg = 'Is after or equal check #02' ).
    ELSE.
      " Works as expected...
    ENDIF.

    o_other = o_this->clone( ).

    IF o_this->is_after_or_equal( io_datetime = o_other iv_ignore_time = abap_false ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is after or equal check #03' ).
    ENDIF.

    o_other->add_seconds( 1 ).

    IF o_this->is_after_or_equal( io_datetime = o_other iv_ignore_time = abap_false ).
      cl_aunit_assert=>fail( msg = 'Is after or equal check #04' ).
    ELSE.
      " Works as expected...
    ENDIF.

    o_other = o_this->clone( )->add_seconds( - 1 ).

    IF o_this->is_after_or_equal( io_datetime = o_other iv_ignore_time = abap_false ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is after or equal check #05' ).
    ENDIF.

  ENDMETHOD.

  METHOD is_equal.

    DATA(o_this)  = zcl_datetime=>from_now( ).
    DATA(o_other) = o_this->clone( ).

    IF o_this->is_equal( o_other ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is equal check #01' ).
    ENDIF.

    o_other->add_days( 1 ).

    IF o_this->is_equal( o_other ).
      cl_aunit_assert=>fail( msg = 'Is equal check #02' ).
    ELSE.
      " Works as expected...
    ENDIF.

    o_other = o_this->clone( )->add_seconds( 1 ).

    IF o_this->is_equal( o_other ).
      " Works as expected...
    ELSE.
      cl_aunit_assert=>fail( msg = 'Is equal check #03' ).
    ENDIF.

    IF o_this->is_equal( io_datetime = o_other iv_ignore_time = abap_false ).
      cl_aunit_assert=>fail( msg = 'Is equal check #04' ).
    ELSE.
      " Works as expected...
    ENDIF.

  ENDMETHOD.

  METHOD create_list.

    DATA(o_this)  = zcl_datetime=>from_date( '20210307' ).

    o_this->begin_of_year( )->begin_of_day( ).

    " Create a list with all 12 month in a year
    DATA(t_datetime_list) = zcl_datetime=>create_list( io_start         = o_this
                                                         iv_increment_field = zcl_datetime=>c_fields-month
                                                         iv_increment_value = 1
                                                         iv_num_of_elements = 12 ).

    cl_aunit_assert=>assert_equals( act = lines( t_datetime_list )
                                    exp = 12
                                    msg = 'Create list check #01' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 1 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210101' )
                                    msg = 'Create list check #02' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 2 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210201' )
                                    msg = 'Create list check #03' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 3 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210301' )
                                    msg = 'Create list check #04' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 4 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210401' )
                                    msg = 'Create list check #05' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 5 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210501' )
                                    msg = 'Create list check #06' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 6 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210601' )
                                    msg = 'Create list check #07' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 7 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210701' )
                                    msg = 'Create list check #08' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 8 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210801' )
                                    msg = 'Create list check #09' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 9 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210901' )
                                    msg = 'Create list check #10' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 10 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20211001' )
                                    msg = 'Create list check #11' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 11 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20211101' )
                                    msg = 'Create list check #12' ).

    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 12 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20211201' )
                                    msg = 'Create list check #13' ).

    " Creating date list with all 31 days of january
    t_datetime_list = zcl_datetime=>create_list( io_start         = o_this
                                                   iv_increment_field = zcl_datetime=>c_fields-day
                                                   iv_increment_value = 1
                                                   iv_num_of_elements = 31 ).

    cl_aunit_assert=>assert_equals( act = lines( t_datetime_list )
                                    exp = 31
                                    msg = 'Create list check #14' ).

    " Testing only one sample
    cl_aunit_assert=>assert_equals( act = t_datetime_list[ 15 ]-o_datetime->as_date( )
                                    exp = CONV dats( '20210115' )
                                    msg = 'Create list check #14' ).

  ENDMETHOD.

  METHOD sub_days.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210307' ).

    o_datetime->sub_days( 5 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210302000000'
                                    msg = 'Sub days check #01' ).

    o_datetime->sub_days( -10 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210312000000'
                                    msg = 'Sub days check #02' ).

  ENDMETHOD.


  METHOD sub_hours.


    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307'  iv_time = '060000' ).

    o_datetime->sub_hours( 8 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210306220000'
                                    msg = 'Sub hours check #01' ).

    o_datetime->sub_hours( 16 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210306060000'
                                    msg = 'Sub hours check #02' ).

    o_datetime->sub_hours( -24 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307060000'
                                    msg = 'Sub hours check #03' ).


  ENDMETHOD.


  METHOD sub_minutes.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307'  iv_time = '060000' ).

    o_datetime->sub_minutes( 8 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307055200'
                                    msg = 'Sub minutes check #01' ).

    o_datetime->sub_minutes( 52 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307050000'
                                    msg = 'Sub minutes check #02' ).

  ENDMETHOD.


  METHOD sub_months.

    DATA(o_datetime) = zcl_datetime=>from_date( '20210131' ).

    DATA(date) = o_datetime->sub_months( 1 )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20201231'
                                    msg = 'Sub month check #01' ).

    date = o_datetime->sub_months( 36 )->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20171231'
                                    msg = 'Sub month check #02' ).


  ENDMETHOD.


  METHOD sub_seconds.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307'  iv_time = '060000' ).

    o_datetime->sub_seconds( 8 ).

    DATA(ts) = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307055952'
                                    msg = 'Sub seconds check #01' ).

    o_datetime->sub_seconds( 52 ).

    ts = o_datetime->as_timestamp( ).

    cl_aunit_assert=>assert_equals( act = ts
                                    exp = '20210307055900'
                                    msg = 'Sub seconds check #02' ).


  ENDMETHOD.


  METHOD sub_weeks.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' ).

    o_datetime->sub_weeks( 1 ).

    DATA(date) = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210228'
                                    msg = 'Sub weeks check #01' ).

    o_datetime->sub_weeks( -2 ).

    date = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20210314'
                                    msg = 'Sub weeks check #02' ).
  ENDMETHOD.


  METHOD sub_years.

    DATA(o_datetime) = zcl_datetime=>from_date( iv_date = '20210307' ).

    o_datetime->sub_years( 1 ).

    DATA(date) = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20200307'
                                    msg = 'Sub years check #01' ).

    o_datetime->sub_years( -2 ).

    date = o_datetime->as_date( ).

    cl_aunit_assert=>assert_equals( act = date
                                    exp = '20220307'
                                    msg = 'Sub years check #02' ).

  ENDMETHOD.

  METHOD diff.

    DATA(o_this) = zcl_datetime=>from_date( iv_date = '20210307' ).
    DATA(o_other) = o_this->clone( )->add_hours( 36 ).


    DATA(diff) = o_this->diff( io_datetime = o_other
                               iv_field      = zcl_datetime=>c_fields-day ).

    cl_aunit_assert=>assert_equals( act = diff
                                    exp = CONV zcl_datetime=>ty_diff( '1.5' )
                                    msg = 'Diff check #01' ).

    diff = o_this->diff( io_datetime = o_other
                         iv_field      = zcl_datetime=>c_fields-day
                         iv_round_mode = zcl_datetime=>c_rouding-round_ceiling ).

    cl_aunit_assert=>assert_equals( act = diff
                                    exp = CONV zcl_datetime=>ty_diff( '2' )
                                    msg = 'Diff check #02' ).

    diff = o_this->diff( io_datetime = o_other
                         iv_field      = zcl_datetime=>c_fields-day
                         iv_round_mode = zcl_datetime=>c_rouding-round_down ).

    cl_aunit_assert=>assert_equals( act = diff
                                    exp = CONV zcl_datetime=>ty_diff( '1' )
                                    msg = 'Diff check #02' ).

    o_other = o_this->clone( )->add_hours( 44 ).

    diff = o_this->diff( io_datetime = o_other
                         iv_field      = zcl_datetime=>c_fields-day ).

    cl_aunit_assert=>assert_equals( act = diff
                                    exp = CONV zcl_datetime=>ty_diff( '1.833333333333333' )
                                    msg = 'Diff check #02' ).

    o_this  = zcl_datetime=>from_date( '20210101' ).
    o_other = zcl_datetime=>from_date( '20211231' ).

    diff = o_this->diff( io_datetime = o_other
                         iv_field      = zcl_datetime=>c_fields-month ).

  ENDMETHOD.

ENDCLASS.
