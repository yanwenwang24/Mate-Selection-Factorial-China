## ------------------------------------------------------------------------
##
## Script name: dictionaries.jl
## Purpose: Dictionaries
## Author: Yanwen Wang
## Date Created: 2025-03-19
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
##
## ------------------------------------------------------------------------

# Demographics ------------------------------------------------------------

# Gender (Q1)
gender_dict = Dict(
    1 => "men",
    2 => "women"
)

# Sexual orientation (Q2)
sexuality_dict = Dict(
    1 => "heter",
    2 => "non-heter"
)

# City (Q4_2)
city_dict = Dict(
    "北京" => "Beijing",
    "上海" => "Shanghai",
    "广州" => "Guangzhou",
    "深圳" => "Shenzhen",
    missing => "Other"
)

# Nativity (Q5)
nativity_dict = Dict(
    1 => "local",
    2 => "migrant",
    missing => missing
)

# Marital status (Q6)
marst_dict = Dict(
    1 => "never-married",
    2 => "married",
    3 => "divorced",
    4 => "widowed"
)

# Romance (Q7)
romance_dict = Dict(
    1 => "never",
    2 => "ever",
    3 => "current"
)

# Cohabitation (Q8)
cohabit_dict = Dict(
    1 => 1,
    2 => 0,
    missing => 0
)

# Ideal age of marriage (Q9)    
agemar_dict = Dict(
    1 => "27 and below",
    2 => "28-30",
    3 => "31-35",
    4 => "36-40",
    5 => "above 40",
    6 => "against marriage"
)

# Hukou at age 12 (Q10)
hukou_dict = Dict(
    1 => "rural",
    2 => "urban"
)

# Parents' education (Q11)
peduc_dict = Dict(
    1 => "primary or less",
    2 => "middle school",
    3 => "high school",
    4 => "some college",
    5 => "college",
    6 => "graduate"
)

# Education (Q12)
educ_dict = Dict(
    1 => "high school or less",
    2 => "some college",
    3 => "college",
    4 => "graduate"
)

# Employment status (Q13)
empstat_dict = Dict(
    1 => "full-time",
    2 => "part-time",
    3 => "student",
    4 => "unemployed",
    5 => "inactive"
)

# Income (Q14)
income_dict = Dict(
    1 => "below 3000",
    2 => "3001-5000",
    3 => "5001-8000",
    4 => "8001-15000",
    5 => "15001-25000",
    6 => "above 25000"
)

# Siblings (Q15)
siblings_dict = Dict(
    1 => 0,
    2 => 1,
    3 => 2
)

# Oldest son/daughter (Q16)
oldest_dict = Dict(
    1 => 1,
    2 => 0,
    missing => 1
)

# Vignettes ---------------------------------------------------------------

v_age_dict = Dict(
    "比您大5岁" => "older",
    "与您年龄相仿" => "similar",
    "比您小5岁" => "younger",
    missing => missing
)

v_educ_dict = Dict(
    "高中文凭" => "secondary",
    "大学本科" => "bachelor",
    "硕士学位" => "graduate",
    missing => missing
)

v_income_dict = Dict(
    "薪资低于平均水平，职业发展空间有限" => "below average",
    "薪资处于平均水平，具备基本职业保障" => "average",
    "薪资高于平均水平，职业晋升通道明确" => "above average",
    missing => missing
)

v_nativity_dict = Dict(
    "在本地出生、长大" => "local",
    "不在本地出生、长大" => "migrant",
    missing => missing
)

v_appearance_dict = Dict(
    "低于大众平均颜值" => "below average",
    "处于大众平均颜值" => "average",
    "高于大众平均颜值" => "above average",
    missing => missing
)

v_housework_dict = Dict(
    "认为家务主要由女性承担" => "women do more",
    "认为家务由双方平等承担" => "equal",
    "认为家务主要由男性承担" => "men do more",
    missing => missing
)

v_career_dict = Dict(
    "对伴侣的事业抱负和发展支持较少" => "low",
    "对伴侣的事业抱负和发展支持一般" => "moderate",
    "对伴侣的事业抱负和发展支持较多" => "strong",
    missing => missing
)

v_emotion_dict = Dict(
    "情绪价值较低，较少主动关心伴侣情绪" => "low",
    "情绪价值一般，能回应伴侣的情感需求" => "moderate",
    "情绪价值较高，能察觉并回应伴侣的情感需求" => "strong",
    missing => missing
)

v_fertility_dict = Dict(
    "明确表示不要孩子" => "no",
    "尚未就是否生育做出决定" => "uncertain",
    "有生育意愿和规划" => "yes",
    missing => missing
)

# Post-vignette assessment ------------------------------------------------

# Fertility (Q26)
pv_fertility_dict = Dict(
    1 => "no",
    2 => "uncertain",
    3 => "yes"
)

# Appearance (Q27)
pv_appearance_dict = Dict(
    1 => "below average",
    2 => "average",
    3 => "above average"
)
