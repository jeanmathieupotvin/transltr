# .hook_load() -----------------------------------------------------------------


test_that(".hook_load() returns null invisibly", {
    # Ensure there is nothing attached to the
    # search path, and call the load hook
    # twice to test both if branches.
    .hook_unload()
    expect_null(.hook_load())
    expect_null(.hook_load())
})

test_that(".hook_load() attaches an environment to the search path", {
    # Reset state.
    .hook_unload()
    .hook_load()

    out <- as.environment(.__STR_ATTACHED_DB)

    expect_type(out, "environment")
    expect_length(out, 0L)
    expect_identical(environmentName(out), .__STR_ATTACHED_DB)
})

test_that(".hook_load() does not attach an environment to the search path if not required", {
    # Reset state.
    .hook_unload()
    .hook_load()

    search_before_second_call <- search()
    .hook_load()
    search_after_second_call <- search()

    expect_identical(search_before_second_call, search_after_second_call)
})


# .hook_unload() ---------------------------------------------------------------


test_that(".hook_unload() returns null invisibly", {
    expect_null(.hook_unload())
})

test_that(".hook_unload() removes an environment from the search path", {
    # Reset state, then unset it.
    .hook_load()
    .hook_unload()
    expect_error(.hook_unload())
})
