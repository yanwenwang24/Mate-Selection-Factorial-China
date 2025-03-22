## ------------------------------------------------------------------------
##
## Script name: 01_answer.jl
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
data_answer.gender = categorical(data_answer.gender, levels = ["men", "women"])

# Sexual orientation (Q2)
transform!(data_answer, :Q2 => ByRow(x -> sexuality_dict[x]) => :sexuality)
data_answer.sexuality = categorical(data_answer.sexuality, levels = ["heter", "non-heter"])

# Birth year and month (Q3)
data_answer.birthy = year.(data_answer.Q3)
data_answer.birthym = month.(data_answer.Q3)

# Age
const_date = Date(2025, 3, 1)
data_answer.age = [
    year(const_date) - year(b) -
    ((month(const_date) < month(b)) || (month(const_date) == month(b) && day(const_date) < day(b)) ? 1 : 0)
    for b in data_answer.Q3
]

# City (Q4)
transform!(data_answer, :Q4 => ByRow(x -> city_dict[x]) => :city)
data_answer.city = categorical(data_answer.city, levels = ["Beijing", "Shanghai", "Guangzhou", "Shenzhen", "Other"])

# Marital status (Q5)
transform!(data_answer, :Q5 => ByRow(x -> marst_dict[x]) => :marst)
data_answer.marst = categorical(data_answer.marst, levels = ["never-married", "married", "divorced", "widowed"])

# Romance (Q6)
transform!(data_answer, :Q6 => ByRow(x -> romance_dict[x]) => :romance)
data_answer.romance = categorical(data_answer.romance, levels = ["never", "ever", "current"])

# Cohabitation (Q7)
transform!(data_answer, :Q7 => ByRow(x -> cohabit_dict[x]) => :cohabit)

# Ideal age of marriage (Q8)
transform!(data_answer, :Q8 => ByRow(x -> agemar_dict[x]) => :agemar)
data_answer.agemar = categorical(data_answer.agemar, levels = ["27 and below", "28-30", "31-35", "36-40", "above 40"])

# Hukou at age 12 (Q9)
transform!(data_answer, :Q9 => ByRow(x -> hukou_dict[x]) => :hukou)
data_answer.hukou = categorical(data_answer.hukou, levels = ["rural", "urban"])

# Parents' education (Q10)
transform!(data_answer, :Q10 => ByRow(x -> peduc_dict[x]) => :peduc)
data_answer.peduc = categorical(data_answer.peduc, levels = ["primary or less", "middle school", "high school", "some college", "college", "graduate"])

# Education (Q11)
transform!(data_answer, :Q11 => ByRow(x -> educ_dict[x]) => :educ)
data_answer.educ = categorical(data_answer.educ, levels = ["high school or less", "some college", "college", "graduate"])

# Employment status (Q12)
transform!(data_answer, :Q12 => ByRow(x -> empstat_dict[x]) => :empstat)
data_answer.empstat = categorical(data_answer.empstat, levels = ["full-time", "part-time", "student", "unemployed", "inactive"])

# Income (Q13)
transform!(data_answer, :Q13 => ByRow(x -> income_dict[x]) => :income)
data_answer.income = categorical(data_answer.income, levels = ["below 3000", "3001-5000", "5001-8000", "8001-15000", "15001-25000", "above 25000"])

# Siblings (Q14)
transform!(data_answer, :Q14 => ByRow(x -> siblings_dict[x]) => :sibling)
data_answer.sibling = categorical(data_answer.sibling, levels = [0, 1, 2])

# Oldest son/daughter (Q15)
transform!(data_answer, :Q15 => ByRow(x -> oldest_dict[x]) => :oldest)

demo_df = select(
    data_answer,
    :id,
    :gender,
    :sexuality,
    :birthy,
    :birthym,
    :age,
    :city,
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

# Housework (Q22)
data_answer.housework_scale = data_answer.Q22

transform!(data_answer, [:gender, :housework_scale] => ByRow( (g, s) ->
    if g == "men"
        s <= 2 ? "women" : s >= 4 ? "men" : "equal"
    else
        s <= 2 ? "men" : s >= 4 ? "women" : "equal"
    end
) => :housework_cat)

data_answer.housework_cat = categorical(data_answer.housework_cat, levels = ["equal", "women", "men"])

# Career support(Q23_1)
data_answer.career_scale = data_answer.Q23_1

transform!(data_answer, :career_scale => ByRow(x ->
    x <= 2 ? "low" : x >= 4 ? "high" : "middle"
) => :career_cat)

data_answer.career_cat = categorical(data_answer.career_cat, levels = ["low", "middle", "high"])

# Emotional support (Q23_2)
data_answer.emotion_scale = data_answer.Q23_2

transform!(data_answer, :emotion_scale => ByRow(x ->
    x <= 2 ? "low" : x >= 4 ? "high" : "middle"
) => :emotion_cat)

data_answer.emotion_cat = categorical(data_answer.emotion_cat, levels = ["low", "middle", "high"])

# Decision-making (Q24)
transform!(data_answer, :Q24 => ByRow(x -> pv_decision_dict[x]) => :decision)
data_answer.decision = categorical(data_answer.decision, levels = ["both", "self", "partner"])

# Fertility (Q25)
transform!(data_answer, :Q25 => ByRow(x -> pv_fertility_dict[x]) => :fertility)
data_answer.fertility = categorical(data_answer.fertility, levels = ["uncertain", "yes", "no"])

# Appearance (Q26)
transform!(data_answer, :Q26 => ByRow(x -> pv_appearance_dict[x]) => :appearance)
data_answer.appearance = categorical(data_answer.appearance, levels = ["average", "ugly", "pretty"])

pv_df = select(
    data_answer,
    :id,
    :housework_scale,
    :housework_cat,
    :career_scale,
    :career_cat,
    :emotion_scale,
    :emotion_cat,
    :decision,
    :fertility,
    :appearance
)

# 3 Vignettes -------------------------------------------------------------

vignette_df = select(data_answer, :id, Between("M1_1_1", "Q21_2"))

# 3.1 Dimensions ----------------------------------------------------------

# Select the dimensions
dim_cols = filter(col -> startswith(col, "M"), names(vignette_df))

stacked_dims = stack(vignette_df, dim_cols, variable_name="dim_col", value_name="dim_value")

stacked_dims[!, :vignette] = [parse(Int, split(col, "_")[1][2:end]) for col in stacked_dims.dim_col]
stacked_dims[!, :dimension] = [parse(Int, split(col, "_")[3]) for col in stacked_dims.dim_col]

long_dims = unstack(stacked_dims, [:id, :vignette], :dimension, :dim_value)

rename!(long_dims,
    Dict(
        "1" => "v_age",
        "2" => "v_educ",
        "3" => "v_income",
        "4" => "v_appearance",
        "5" => "v_housework",
        "6" => "v_career",
        "7" => "v_emotion",
        "8" => "v_decision",
        "9" => "v_fertility"
    )
)

# Recode the dimensions
transform!(long_dims, :v_age => ByRow(x -> v_age_dict[x]) => :v_age)
transform!(long_dims, :v_educ => ByRow(x -> v_educ_dict[x]) => :v_educ)
transform!(long_dims, :v_income => ByRow(x -> v_income_dict[x]) => :v_income)
transform!(long_dims, :v_appearance => ByRow(x -> v_appearance_dict[x]) => :v_appearance)
transform!(long_dims, :v_housework => ByRow(x -> v_housework_dict[x]) => :v_housework)
transform!(long_dims, :v_career => ByRow(x -> v_career_dict[x]) => :v_career)
transform!(long_dims, :v_emotion => ByRow(x -> v_emotion_dict[x]) => :v_emotion)
transform!(long_dims, :v_decision => ByRow(x -> v_decision_dict[x]) => :v_decision)
transform!(long_dims, :v_fertility => ByRow(x -> v_fertility_dict[x]) => :v_fertility)

# Categorize the dimensions
long_dims.v_age = categorical(long_dims.v_age, levels = ["same", "older", "younger"])
long_dims.v_educ = categorical(long_dims.v_educ, levels = ["low", "middle", "high"])
long_dims.v_income = categorical(long_dims.v_income, levels = ["low", "middle", "high"])
long_dims.v_appearance = categorical(long_dims.v_appearance, levels = ["average", "ugly", "pretty"])
long_dims.v_housework = categorical(long_dims.v_housework, levels = ["equal", "women", "men"])
long_dims.v_career = categorical(long_dims.v_career, levels = ["low", "middle", "high"])
long_dims.v_emotion = categorical(long_dims.v_emotion, levels = ["low", "middle", "high"])
long_dims.v_decision = categorical(long_dims.v_decision, levels = ["both", "self", "partner"])
long_dims.v_fertility = categorical(long_dims.v_fertility, levels = ["uncertain", "yes", "no"])

# 3.2 Responses -----------------------------------------------------------

# Select the responses
resp_cols = filter(col -> startswith(col, "Q"), names(vignette_df))

stacked_resp = stack(vignette_df, resp_cols, variable_name = "resp_col", value_name = "resp_value")

function parse_response_info(colname::String)
    parts = split(colname, "_")
    # Remove the leading "Q" and subtract 15 to get the vignette number.
    vignette = parse(Int, replace(parts[1], "Q" => "")) - 15
    response_number = parse(Int, parts[2])
    return (vignette, response_number)
end

info = [parse_response_info(c) for c in stacked_resp.resp_col]
stacked_resp[!, :vignette] = getindex.(info, 1)
stacked_resp[!, :response_num] = getindex.(info, 2)

long_resp = unstack(stacked_resp, [:id, :vignette], :response_num, :resp_value)
rename!(long_resp, Dict("1" => "r_date", "2" => "r_marriage"))

# Merge the dimensions and responses
vignette_df = innerjoin(long_dims, long_resp, on = [:id, :vignette])

# 4 Merge -----------------------------------------------------------------

data_answer = innerjoin(innerjoin(demo_df, pv_df, on = :id), vignette_df, on = :id)

sort!(data_answer, [:id, :vignette])

# Save
Arrow.write("data_clean/data_answer.arrow", data_answer)
