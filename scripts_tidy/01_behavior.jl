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

# 1 IP address ------------------------------------------------------------

# ID
rename!(data_answer, "\ufeffid" => "id")

# IP address
ip_address = select(
    data_answer,
    :id,
    :ip_country,
    :ip_province,
    :ip_city
)

# 2 Duration for demographic questions ------------------------------------

# ID
rename!(data_behavior, "\ufeffid" => "id")

# Duration for demographic questions
demo_behavior = select(data_behavior, :id, r"^page([2-9]|1[0-5])_duration$")

# Total duration
demo_behavior.demo_duration = [sum(row) for row in eachrow(demo_behavior[:, 2:end])]

select!(demo_behavior, :id, :demo_duration)

# 3 Duration for vignettes ------------------------------------------------

vignette_behavior = select(data_behavior, :id, r"^page(1[6-9]|2[0-1])_duration$")

# Rename
rename!(
    vignette_behavior,
    Dict(
        "page16_duration" => "v1_duration",
        "page17_duration" => "v2_duration",
        "page18_duration" => "v3_duration",
        "page19_duration" => "v4_duration",
        "page20_duration" => "v5_duration",
        "page21_duration" => "v6_duration"
    )
)

# Total duration
vignette_behavior.vignette_duration = [sum(row) for row in eachrow(vignette_behavior[:, 2:end])]

# 4 Post-vignette questions -----------------------------------------------

post_behavior = select(data_behavior, :id, :page22_duration => :post_duration)

# 5 Merge -----------------------------------------------------------------

data_behavior = innerjoin(ip_address, demo_behavior, on = :id)
data_behavior = innerjoin(data_behavior, vignette_behavior, on = :id)
data_behavior = innerjoin(data_behavior, post_behavior, on = :id)

data_behavior.total_duration = data_behavior.demo_duration + data_behavior.vignette_duration + data_behavior.post_duration

# Save
Arrow.write("data_clean/data_behavior.arrow", data_behavior)
