#!/usr/bin/perl -w
#$Id$
use Finance::YahooQuote;
my @symbols = <>;
chomp (@symbols);
my @labels=qw(
              Symbol
              Company_Name
              Last_Price
              Last_Trade_Date
              Last_Trade_Time
              Change
              Percent_Change
              Volume
              Average_Daily_Vol
              Bid
              Ask
               Previous_Close
              Open
              Day_Range
              52-Week_Range
              Earnings_per_Share
              P/E
              Dividend_Pay_Date
              Dividend_per_Share
              Dividend_Yield
              Market_Capitalization
              Stock_Exchange
             );


@quotes = getquote @symbols;
print join (", ", @labels ),"\n";
foreach $q (@quotes ) {
print join(", ", @$q),"\n";
}
