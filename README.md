# CPC351-Project

## Dataset 1: Weather Sensor Data

Micro-climate sensors collect telemetry at set intervals throughout the day. Sensors are located at various locations in the City of Canning, Western Australia and each sensor has a unique ID. Contact us at opendata@canning.wa.gov.au for a larger data set (The data supplied is the sensor reading for 30 days).

### Sensor Locations

| Device ID    | Location                                        |
|--------------|------------------------------------------------|
| 18zua9muwbb  | Wharf Street Basin - Pavilion                  |
| 2hq3byfebne  | The City's Civic and Administration Building   |
| uu90853psl   | Wharf Street Basin - Leila Street entrance     |
<!-- | xd2su7w05m   | Wharf Street Basin - Nature Play Area          | -->

### Weather Data Features (06_Weather.csv)

| Column Name           | Description                                                                                    | Data Type |
|----------------------|------------------------------------------------------------------------------------------------|-----------|
| SID                  | Unique identifier for each record (composite of timestamp and device ID)                        | Text      |
| DeviceID             | Identifier for the device that recorded the weather data                                        | Text      |
| ObservationTimestamp | UTC timestamp of when the observation was recorded (MM/DD/YYYY HH:MM)                          | DateTime  |
| LocalTimestamp       | Local time (UTC+8) corresponding to the observation timestamp                                   | DateTime  |
| AirTemperature       | Temperature of the air in degrees Celsius                                                       | Numeric   |
| BarometricPressure   | Atmospheric pressure in hectopascals (hPa)                                                     | Numeric   |
| RelativeHumidity     | Percentage of humidity in the air relative to maximum moisture capacity at that temperature     | Numeric   |

## Dataset 2: Social Housing Data

This dataset contains details of applications for social housing in Australia, as of 30 June. It includes various attributes related to the application process and applicant needs.

### Social Housing Features (06_SocialHousing.csv)

| Column Name | Description | Data Type | Values/Format |
|------------|-------------|-----------|---------------|
| row_id | A unique number for each row of this dataset | Numeric | |
| ApplicationListCategory | Level of housing need | Text | |
| Housing Service Centre | Housing Service Centre where the application was lodged | Text | |
| LocalGovtAuthority | Local government authority associated with the applicant household's first locational preference | Text | |
| StateElectorate | State electorate associated with the applicant household's first locational preference | Text | |
| ApplicationListStatusDesc | Approval status of the housing register application | Text | - Active: An applicant is ready to be offered social housing should it be available (Status formerly coded as approved)<br>- Inactive: An applicant is self-nominated and are those people who are not yet ready to accept a social home due to their personal circumstances (Status formerly coded as deferred) |
| RehousingListDesc | If the application is a new application or an application for a transfer | Text | |
| LanguagePreference | Language used at interview | Text | n.a. = data unavailable |
| BedroomsRequired | Number of bedrooms the applicant household is entitled to | Numeric | |
| ApplicationType | Housing programs the applicant household has listed for | Text | - Public/CH = Public Rental Housing/Community Housing<br>- Affordable = Affordable Housing<br>- Transitional = Transitional Housing<br>- ATSIH = Aboriginal and Torres Strait Islander Housing<br>- RIC = Remote Indigenous Community<br>- n.a. = data unavailable |
| ApplicationReceivedMonth | Month the application was approved | Text | MMM-YY |
| MonthsOnHousingRegister | Number of months the applicant household has been registered | Numeric | |
| FamilyType | Family type of applicant household | Text | |
| PeopleonApplication | Number of people listed on the application | Numeric | |
| LettingArea1Desc-LettingArea6Desc | Listed locational preference | Text | n.a. = no preference expressed or data unavailable for reasons of confidentiality |
| DisabilityModRequestFlag | Does the household request disability modifications? | Text | - Yes<br>- No<br>- n.a. = data unavailable for reasons of confidentiality |
| AtRiskOfOrExperiencingHomelessnessFlag | Was the household homeless or at risk of homelessness at time of application? | Text | - Yes<br>- No<br>- U = Unspecified<br>- n.a. = data unavailable for reasons of confidentiality |
| DisabilityApplicationFlag | Does the household contain a person with an identified disability? | Text | - Yes<br>- No<br>- U = Unspecified<br>- n.a. = data unavailable for reasons of confidentiality |
| IndigenousApplicationFlag | Has any member of the household identified as being Aboriginal or Torres Strait Islander? | Text | - Yes<br>- No<br>- U = Unspecified<br>- n.a. = data unavailable for reasons of confidentiality |



