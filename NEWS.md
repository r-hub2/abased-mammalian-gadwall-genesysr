## version 2.1.1

- Fixed issue with user_login() with httr2.

## version 2.1.0

- Improved support for logging into Genesys with OAuth 2.1
- Fixes issue with `setup_sandbox()` and `user_login()` that resulted in `invalid_client` error

## version 2.0.0

- Upgraded `user_login()` for OAuth 2.1

## version 1.1.0

- Added list_crops(), list_species() and list_institutes()
- Added support to exclude fields in get_accessions()

## version 1.0.1

- Updated API calls to validator.genesys-pgr.org with `escapeChar`
- Better handling of CSV response from validator

## version 1.0.0

- Updated from Genesys API v0 calls to API v1.

### Notes

The filter structure has changed. See Tutorial.

## version 0.9

- August 2019: Updated for API at api.genesys-pgr.org
- June 2018: First version of the Genesys API client for R is released.

