def fix_manifest_order(obj):
    # Not sure why rtoml is not honoring this order even for pure load/save cycles... oh well

    delayed = dict()

    # extract tables/table arrays:
    for key in obj:
        val = obj[key]
        if isinstance(val, dict) or \
                (isinstance(val, list) and len(val) > 0 and isinstance(val[0], dict)):
            delayed[key] = val

    # reinsert at last position
    for key in delayed:
        obj.pop(key)
        obj[key] = delayed[key]

    return obj
