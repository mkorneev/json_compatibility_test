# JSON Backwards Compatibility Test Tool

A command line tool to test if one JSON file is backwards compatible with another.

By default new names in objects are allowed, arrays should be of the same length and values should match exactly.

This can be overridden by providing a *specification* file.

## Specification format

Specification resembles structure of the original JSON file.

```json
[
    {                                            // describes the array itself
        "@allow_adding_items": true | false,     // false by default
        "@allow_removing_items": true | false,   // false by default
        "@index_by": "id",                       // sort array items by this field (null by default)
        "@compare_by": "name",                   // deem two object equal if their "name" fields are equal
    },
    {                                            // describes array items
        "@allow_adding_names": true | false,     // true by default
        "@allow_removing_names": true | false,   // false by default
        "name1": "@ignore_name",                 // this name can be absent in the new version
        "name2": "@ignore_value",                // do not check this value for equality
        "name3": { "@abs": 10 },                 // value must be a number and must not differ for more than 10 from the original value
        "name4": { "@rel": 0.1 },                // value must be a number and must not differ for more than 10% of the original value
        "name5": { "@time": "10 sec" },          // value must be an ISO 8601 timestamp and must not differ for more than 10 seconds from the original value
        "name6": { "@time": "5 min" },           // value must be an ISO 8601 timestamp and must not differ for more than 5 minutes from the original value
    }
]
```

## Usage example

TBD