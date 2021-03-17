# History of bankster releases

## 1.0.5 (2021-03-18)

- Fixed a bug reversing order of weighted currencies.

## 1.0.4 (2021-03-18)

- Added missing Monetary implementation functions for Money.
- Improved currency/id Monetary implementation to support currency codes.

## 1.0.3 (2021-03-17)

- Fixed a bug that caused wrong locale detection on some environments,
  caused by redundant localized properties preparation (double parsing
  of locale object initially expressed with a keyword).

## 1.0.2 (2021-03-16)

- Currency codes are now default fallback identifiers of registered currencies.
- Fixed (re-)setting of localized properties during currency registering.

## 1.0.1 (2021-03-15)

- Removed accidental databases from the resources.

## 1.0.0 (2021-03-15)

- Initial release.

