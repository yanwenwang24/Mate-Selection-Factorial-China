## ------------------------------------------------------------------------
##
## Script name: 02_answer.jl
## Purpose: Answers to the survey questions
## Author: Yanwen Wang
## Date Created: 2025-03-19
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# 1 Demographics ----------------------------------------------------------

# Gender (Q1)
transform!(data_answer, :Q1 => ByRow(x -> gender_dict[x]) => :gender)
data_answer.gender = categorical(data_answer.gender, levels=["men", "women"])

# Sexual orientation (Q2)
transform!(data_answer, :Q2 => ByRow(x -> sexuality_dict[x]) => :sexuality)
data_answer.sexuality = categorical(
    data_answer.sexuality,
    levels=["heter", "non-heter"]
)

# Birth year and month (Q3)
data_answer.birthy = year.(data_answer.Q3)
data_answer.birthym = month.(data_answer.Q3)

# Age
const_date = Date(2025, 4, 1)
data_answer.age = [
    year(const_date) - year(b) -
    (
        (month(const_date) < month(b)) ||
        (month(const_date) == month(b) &&
         day(const_date) < day(b)) ? 1 : 0
    )
    for b in data_answer.Q3
]

# City (Q4_2)
transform!(data_answer, :Q4_2 => ByRow(x -> city_dict[x]) => :city)
data_answer.city = categorical(
    data_answer.city,
    levels=["Beijing", "Shanghai", "Guangzhou", "Shenzhen", "Other"]
)

# Nativity (Q5)
transform!(data_answer, :Q5 => ByRow(x -> nativity_dict[x]) => :nativity)
data_answer.nativity = categorical(
    data_answer.nativity,
    levels=["local", "migrant"]
)

# Marital status (Q6)
transform!(data_answer, :Q6 => ByRow(x -> marst_dict[x]) => :marst)
data_answer.marst = categorical(
    data_answer.marst,
    levels=["never-married", "married", "divorced", "widowed"]
)

# Romance (Q7)
transform!(data_answer, :Q7 => ByRow(x -> romance_dict[x]) => :romance)
data_answer.romance = categorical(
    data_answer.romance,
    levels=["never", "ever", "current"]
)

# Cohabitation (Q8)
transform!(data_answer, :Q8 => ByRow(x -> cohabit_dict[x]) => :cohabit)

# Ideal age of marriage (Q9)
transform!(data_answer, :Q9 => ByRow(x -> agemar_dict[x]) => :agemar)
data_answer.agemar = categorical(
    data_answer.agemar,
    levels=[
        "27 and below",
        "28-30",
        "31-35",
        "36-40",
        "above 40",
        "against marriage"
    ]
)

# Hukou at age 12 (Q10)
transform!(data_answer, :Q10 => ByRow(x -> hukou_dict[x]) => :hukou)
data_answer.hukou = categorical(
    data_answer.hukou,
    levels=["rural", "urban"]
)

# Parents' education (Q11)
transform!(data_answer, :Q11 => ByRow(x -> peduc_dict[x]) => :peduc)
data_answer.peduc = categorical(
    data_answer.peduc,
    levels=[
        "primary or less",
        "middle school",
        "high school",
        "some college",
        "college",
        "graduate"
    ]
)

# Education (Q12)
transform!(data_answer, :Q12 => ByRow(x -> educ_dict[x]) => :educ)
data_answer.educ = categorical(
    data_answer.educ,
    levels=["high school or less", "some college", "college", "graduate"]
)

# Employment status (Q13)
transform!(data_answer, :Q13 => ByRow(x -> empstat_dict[x]) => :empstat)
data_answer.empstat = categorical(
    data_answer.empstat,
    levels=["full-time", "part-time", "student", "unemployed", "inactive"]
)

# Income (Q14)
transform!(data_answer, :Q14 => ByRow(x -> income_dict[x]) => :income)
data_answer.income = categorical(
    data_answer.income,
    levels=[
        "below 3000",
        "3001-5000",
        "5001-8000",
        "8001-15000",
        "15001-25000",
        "above 25000"
    ]
)

# Siblings (Q15)
transform!(data_answer, :Q15 => ByRow(x -> siblings_dict[x]) => :sibling)
data_answer.sibling = categorical(
    data_answer.sibling,
    levels=[0, 1, 2]
)

# Oldest son/daughter (Q16)
transform!(data_answer, :Q16 => ByRow(x -> oldest_dict[x]) => :oldest)

demo_df = select(
    data_answer,
    :id,
    :gender,
    :sexuality,
    :birthy,
    :birthym,
    :age,
    :city,
    :nativity,
    :marst,
    :romance,
    :cohabit,
    :agemar,
    :hukou,
    :peduc,
    :educ,
    :empstat,
    :income,
    :sibling,
    :oldest
)

# 2 Post-vignette assessment ----------------------------------------------

# Housework (Q24)
data_answer.housework_scale = data_answer.Q24

transform!(data_answer, [:gender, :housework_scale] => ByRow((g, s) ->
    if g == "men"
        s <= 3 ? "women do more" : s >= 5 ? "men do more" : "equal"
    else
        s <= 3 ? "men do more" : s >= 5 ? "women do more" : "equal"
    end
) => :housework_cat)

data_answer.housework_cat = categorical(
    data_answer.housework_cat,
    levels=["equal", "women do more", "men do more"]
)

# Career support(Q25_1)
data_answer.career_scale = data_answer.Q25_1

transform!(data_answer, :career_scale => ByRow(x ->
    x <= 4 ? "low" : x >= 6 ? "strong" : "moderate"
) => :career_cat)

data_answer.career_cat = categorical(
    data_answer.career_cat,
    levels=["moderate", "low", "strong"]
)

# Emotional support (Q25_2)
data_answer.emotion_scale = data_answer.Q25_2

transform!(data_answer, :emotion_scale => ByRow(x ->
    x <= 4 ? "low" : x >= 6 ? "strong" : "moderate"
) => :emotion_cat)

data_answer.emotion_cat = categorical(
    data_answer.emotion_cat,
    levels=["moderate", "low", "strong"]
)

# Fertility (Q26)
transform!(data_answer, :Q26 => ByRow(x -> pv_fertility_dict[x]) => :fertility)
data_answer.fertility = categorical(
    data_answer.fertility,
    levels=["uncertain", "yes", "no"]
)

# Appearance (Q27)
transform!(
    data_answer,
    :Q27 => ByRow(x -> pv_appearance_dict[x]) => :appearance
)
data_answer.appearance = categorical(
    data_answer.appearance,
    levels=["average", "below average", "above average"]
)

# Media time per day (S1)
data_answer.media_time = data_answer.S1

# Media compare (S2)
data_answer.media_compare_income = data_answer.S2_1
data_answer.media_compare_appearance = data_answer.S2_2
data_answer.media_compare_educ = data_answer.S2_3

# Media mobility (S3)
data_answer.media_mobility_income = data_answer.S4
data_answer.media_mobility_appearance = data_answer.S5
data_answer.media_mobility_educ = data_answer.S6

pv_df = select(
    data_answer,
    :id,
    :housework_scale,
    :housework_cat,
    :career_scale,
    :career_cat,
    :emotion_scale,
    :emotion_cat,
    :fertility,
    :appearance,
    :media_time,
    :media_compare_income,
    :media_compare_appearance,
    :media_compare_educ,
    :media_mobility_income,
    :media_mobility_appearance,
    :media_mobility_educ
)

# 3 Vignettes -------------------------------------------------------------

vignette_df = select(data_answer, :id, Between("M1_1_1", "Q23_2"))

# 3.1 Dimensions ----------------------------------------------------------

# Select the dimensions
dim_cols = filter(col -> startswith(col, "M"), names(vignette_df))

stacked_dims = stack(
    vignette_df,
    dim_cols,
    variable_name="dim_col",
    value_name="dim_value"
)

stacked_dims[!, :order] = [
    parse(Int, split(col, "_")[1][2:end]) for col in stacked_dims.dim_col
]
stacked_dims[!, :dimension] = [
    parse(Int, split(col, "_")[3]) for col in stacked_dims.dim_col
]

long_dims = unstack(stacked_dims, [:id, :order], :dimension, :dim_value)

rename!(long_dims,
    Dict(
        "1" => "v_age",
        "2" => "v_educ",
        "3" => "v_income",
        "4" => "v_nativity",
        "5" => "v_appearance",
        "6" => "v_housework",
        "7" => "v_career",
        "8" => "v_emotion",
        "9" => "v_fertility"
    )
)

# Recode the dimensions
transform!(
    long_dims,
    :v_age => ByRow(x -> v_age_dict[x]) => :v_age
)
transform!(
    long_dims,
    :v_educ => ByRow(x -> v_educ_dict[x]) => :v_educ
)
transform!(
    long_dims,
    :v_income => ByRow(x -> v_income_dict[x]) => :v_income
)
transform!(
    long_dims,
    :v_nativity => ByRow(x -> v_nativity_dict[x]) => :v_nativity
)
transform!(
    long_dims,
    :v_appearance => ByRow(x -> v_appearance_dict[x]) => :v_appearance
)
transform!(
    long_dims,
    :v_housework => ByRow(x -> v_housework_dict[x]) => :v_housework
)
transform!(
    long_dims,
    :v_career => ByRow(x -> v_career_dict[x]) => :v_caree
)
transform!(
    long_dims,
    :v_emotion => ByRow(x -> v_emotion_dict[x]) => :v_emotion
)
transform!(
    long_dims,
    :v_fertility => ByRow(x -> v_fertility_dict[x]) => :v_fertility
)

# Categorize the dimensions
long_dims.v_age = categorical(
    long_dims.v_age,
    levels=["similar", "older", "younger"]
)
long_dims.v_educ = categorical(
    long_dims.v_educ,
    levels=["secondary", "bachelor", "graduate"]
)
long_dims.v_income = categorical(
    long_dims.v_income,
    levels=["average", "below average", "above average"]
)
long_dims.v_nativity = categorical(
    long_dims.v_nativity,
    levels=["local", "migrant"]
)
long_dims.v_appearance = categorical(
    long_dims.v_appearance,
    levels=["average", "below average", "above average"]
)
long_dims.v_housework = categorical(
    long_dims.v_housework,
    levels=["equal", "women do more", "men do more"]
)
long_dims.v_career = categorical(
    long_dims.v_career,
    levels=["moderate", "low", "strong"]
)
long_dims.v_emotion = categorical(
    long_dims.v_emotion,
    levels=["moderate", "low", "strong"]
)
long_dims.v_fertility = categorical(
    long_dims.v_fertility,
    levels=["uncertain", "yes", "no"]
)

# 3.2 Responses -----------------------------------------------------------

# Select the responses
resp_cols = filter(col -> startswith(col, "Q"), names(vignette_df))

stacked_resp = stack(
    vignette_df,
    resp_cols,
    variable_name="resp_col",
    value_name="resp_value"
)

function parse_response_info(colname::String)
    parts = split(colname, "_")
    # Remove the leading "Q" and subtract 17 to get the vignette order.
    order = parse(Int, replace(parts[1], "Q" => "")) - 17
    response_number = parse(Int, parts[2])
    return (order, response_number)
end

info = [parse_response_info(c) for c in stacked_resp.resp_col]
stacked_resp[!, :order] = getindex.(info, 1)
stacked_resp[!, :response_num] = getindex.(info, 2)

long_resp = unstack(stacked_resp, [:id, :order], :response_num, :resp_value)
rename!(long_resp, Dict("1" => "r_date", "2" => "r_marriage"))

# Merge the dimensions and responses
vignette_df = innerjoin(long_dims, long_resp, on=[:id, :order])

# 4 Merge -----------------------------------------------------------------

data_answer = innerjoin(innerjoin(demo_df, pv_df, on=:id), vignette_df, on=:id)

sort!(data_answer, [:id, :order])

# Save
Arrow.write("data/processed/data_answer.arrow", data_answer)
