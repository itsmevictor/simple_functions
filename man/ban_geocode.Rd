% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ban_geocode.R
\name{ban_geocode}
\alias{ban_geocode}
\title{Geocode French Addresses Using the BAN API}
\usage{
ban_geocode(query, timeout_sec = 10, types = NULL)
}
\arguments{
\item{query}{A character string containing the address or location to geocode.}

\item{timeout_sec}{Numeric. The timeout in seconds for the HTTP request. Default is 10.}

\item{types}{Character vector or NULL. Specific types to search for (e.g., "municipality", "locality").
If NULL, performs a general search. Default is NULL.}
}
\value{
A data frame containing geocoding results with columns for location
properties including coordinates, scores, and administrative information.
Returns a data frame with NA values if no results are found or if an error occurs.
}
\description{
This function queries the French Base Adresse Nationale (BAN) API to geocode
French addresses. It can search for specific types of locations or perform
a general search.
}
\details{
This function uses the French BAN (Base Adresse Nationale) API to geocode addresses.
The API returns detailed information including coordinates, administrative codes,
and confidence scores. When multiple types are specified, the function returns
the highest-scoring result across all types.
}
\examples{
\dontrun{
# Basic geocoding
result <- ban_geocode("8 Boulevard du Port 80000 Amiens")

# Search for specific types
result <- ban_geocode("Amiens", types = c("municipality", "locality"))
}

}
