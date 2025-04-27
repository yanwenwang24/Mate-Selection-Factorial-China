## ------------------------------------------------------------------------
##
## Script name: 01_behavior.jl
## Purpose: Data for answering behaviors
## Author: Yanwen Wang
## Date Created: 2025-03-20
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes: IP address (country, province, city) and duration for demographic
## questions, vignettes, and post-vignette questions
##
## ------------------------------------------------------------------------

# 1 IP address and source -------------------------------------------------

# ID
rename!(data_answer, "\ufeffid" => "id")

# IP address & source
ip_address = select(
    data_answer,
    :id,
    :ip_country,
    :ip_province,
    :ip_city,
    :send_type => :source
)

# 2 Duration for demographic questions ------------------------------------

# ID
rename!(data_behavior, "\ufeffid" => "id")

# Duration for demographic questions (pages 1-17)
demo_behavior = select(data_behavior, :id, r"^page([1-9]|1[0-7])_duration$")

# Total duration
demo_behavior.demo_duration = [
    sum(skipmissing(row)) for row in eachrow(demo_behavior[:, 2:end])
]

select!(demo_behavior, :id, :demo_duration)

# 3 Duration for vignettes ------------------------------------------------

# Duration for vignettes (pages 18-23)
vignette_behavior = select(
    data_behavior,
    :id,
    r"^page(1[8-9]|2[0-3])_duration$"
)

# Rename
rename!(
    vignette_behavior,
    Dict(
        "page18_duration" => "v1_duration",
        "page19_duration" => "v2_duration",
        "page20_duration" => "v3_duration",
        "page21_duration" => "v4_duration",
        "page22_duration" => "v5_duration",
        "page23_duration" => "v6_duration"
    )
)

# Total duration
vignette_behavior.vignette_duration = [
    sum(skipmissing(row)) for row in eachrow(vignette_behavior[:, 2:end])
]

# 4 Post-vignette questions -----------------------------------------------

# Duration for post-vignette and social media assessments (pages 24-25)
post_behavior = select(data_behavior, :id, r"^page(2[4-5])_duration$")

post_behavior.post_duration = [
    sum(skipmissing(row)) for row in eachrow(post_behavior[:, 2:end])
]

select!(post_behavior, :id, :post_duration)

# 5 Merge -----------------------------------------------------------------

data_behavior = innerjoin(ip_address, demo_behavior, on=:id)
data_behavior = innerjoin(data_behavior, vignette_behavior, on=:id)
data_behavior = innerjoin(data_behavior, post_behavior, on=:id)

data_behavior.total_duration = data_behavior.demo_duration +
                               data_behavior.vignette_duration +
                               data_behavior.post_duration

# Save
Arrow.write("data/processed/data_behavior.arrow", data_behavior)
