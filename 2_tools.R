## ----tables--------------------------------------------------------------
# 3D table - Admit on rows (D1), Gender on columns (D2)
# and Department on levels (D3)
UCBAdmissions # Department has 6 levels

# In 2D - collapse over department
margin.table(UCBAdmissions, 2:1)

# pivot the department onto columns
aperm(UCBAdmissions, c(2, 3, 1))

# flatten to a crosstab in default order
structable(UCBAdmissions)

# flatten to a crosstab with pivot
structable(aperm(UCBAdmissions, c(2, 3, 1)))

# More D
structable(Survived~., data=Titanic)

# More D - another pivot
structable(Survived+Sex~., data=Titanic)

## ----crosstabs-----------------------------------------------------------
# An old fashioned crosstab - 2D
CrossTable(margin.table(UCBAdmissions, 2:1)
           , prop.chisq = FALSE
           , format = "SPSS")
