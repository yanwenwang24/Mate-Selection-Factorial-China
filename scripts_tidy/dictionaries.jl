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

# City (Q4)
city_dict = Dict(
    1 => "Beijing",
    2 => "Shanghai",
    3 => "Guangzhou",
    4 => "Shenzhen",
    5 => "Other"
)

# Marital status (Q5)
marst_dict = Dict(
    1 => "never-married",
    2 => "married",
    3 => "divorced",
    4 => "widowed"
)

# Romance (Q6)
romance_dict = Dict(
    1 => "never",
    2 => "ever",
    3 => "current"
)

# Cohabitation (Q7)
cohabit_dict = Dict(
    1 => 1,
    2 => 0,
    missing => 0
)

# Ideal age of marriage (Q8)    
agemar_dict = Dict(
    1 => "27 and below",
    2 => "28-30",
    3 => "31-35",
    4 => "36-40",
    5 => "above 40"
)

# Hukou at age 12 (Q9)
hukou_dict = Dict(
    1 => "rural",
    2 => "urban"
)

# Parents' education (Q10)
peduc_dict = Dict(
    1 => "primary or less",
    2 => "middle school",
    3 => "high school",
    4 => "some college",
    5 => "college",
    6 => "graduate"
)

# Education (Q11)
educ_dict = Dict(
    1 => "high school or less",
    2 => "some college",
    3 => "college",
    4 => "graduate"
)

# Employment status (Q12)
empstat_dict = Dict(
    1 => "full-time",
    2 => "part-time",
    3 => "student",
    4 => "unemployed",
    5 => "inactive"
)

# Income (Q13)
income_dict = Dict(
    1 => "below 3000",
    2 => "3001-5000",
    3 => "5001-8000",
    4 => "8001-15000",
    5 => "15001-25000",
    6 => "above 25000"
)

# Siblings (Q14)
siblings_dict = Dict(
    1 => 0,
    2 => 1,
    3 => 2
)

# Oldest son/daughter (Q15)
oldest_dict = Dict(
    1 => 1,
    2 => 0,
    missing => 1
)

# Vignettes ---------------------------------------------------------------

v_age_dict = Dict(
    "比您大5岁" => "older",
    "与您年龄相仿" => "same",
    "比您小5岁" => "younger"
)

v_educ_dict = Dict(
    "高中文凭" => "low",
    "大学本科" => "middle",
    "硕士学位" => "high"
)

v_income_dict = Dict(
    "薪资低于平均水平，职业发展空间有限" => "low",
    "薪资处于平均水平，具备基本职业保障" => "middle",
    "薪资高于平均水平，职业晋升通道明确" => "high"
)

v_appearance_dict = Dict(
    "低于大众平均颜值" => "ugly",
    "处于大众平均颜值" => "average",
    "高于大众平均颜值" => "pretty"
)

v_housework_dict = Dict(
    "认为家务主要由女性承担" => "women",
    "认为家务由双方平等承担" => "equal",
    "认为家务主要由男性承担" => "men"
)

v_career_dict = Dict(
    "对伴侣的事业抱负和发展支持较少" => "low",
    "对伴侣的事业抱负和发展支持一般" => "middle",
    "对伴侣的事业抱负和发展支持较多" => "high"
)

v_emotion_dict = Dict(
    "情绪价值较低，较少主动关心伴侣情绪" => "low",
    "情绪价值一般，能回应伴侣的情感需求" => "middle",
    "情绪价值较高，能察觉并回应伴侣的情感需求" => "high"
)

v_decision_dict = Dict(
    "自己主导家庭重大决策" => "self",
    "与伴侣共同商议家庭重大决策" => "both",
    "由伴侣主导家庭重大决策" => "partner"
)

v_fertility_dict = Dict(
    "明确表示不要孩子" => "no",
    "尚未就是否生育做出决定" => "uncertain",
    "有生育意愿和规划" => "yes"
)

# Post-vignette assessment ------------------------------------------------

pv_decision_dict = Dict(
    1 => "self",
    2 => "both",
    3 => "partner"
)

pv_fertility_dict = Dict(
    1 => "no",
    2 => "uncertain",
    3 => "yes"
)

pv_appearance_dict = Dict(
    1 => "ugly",
    2 => "average",
    3 => "pretty"
)

