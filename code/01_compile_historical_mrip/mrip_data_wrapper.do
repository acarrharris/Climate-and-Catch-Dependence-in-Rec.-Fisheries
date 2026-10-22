
/******************************************************************************
Locate MRIP files and pull into memory

OpenAI ChatGPT (GPT-5.6 Sol) was used to optimize this code. 
*******************************************************************************/

global yearlist 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024
global wavelist 1 2 3 4 5 6

global triplist
global catchlist

foreach year of global yearlist {
    foreach wave of global wavelist {
        local trip_source "${mrip_data_root}/trip_`year'`wave'.dta"
        local catch_source "${mrip_data_root}/catch_`year'`wave'.dta"

        capture confirm file "`trip_source'"
        if !_rc {
            use "`trip_source'", clear
            renvarlab, lower
            quietly count

            if r(N) > 0 {
                save "`trip_source'", replace
                global triplist `"$triplist "`trip_source'""'
            }
        }

        capture confirm file "`catch_source'"
        if !_rc {
            use "`catch_source'", clear
            renvarlab, lower
            quietly count

            if r(N) > 0 {
                save "`catch_source'", replace
                global catchlist `"$catchlist "`catch_source'""'
            }
        }
    }
}

if `"$triplist"' == "" {
    display as error "No nonempty MRIP trip files were found in ${mrip_data_root}."
    exit 601
}

if `"$catchlist"' == "" {
    display as error "No nonempty MRIP catch files were found in ${mrip_data_root}."
    exit 601
}

display as text "MRIP input lists created successfully."