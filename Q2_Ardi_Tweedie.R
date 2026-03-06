library(insuranceData)

data(dataCar)
df2 <- data.frame(dataCar)

library(GJRM)

## Fitting the marginal distributions

# Fit Poisson for the claim frequency 
P.freq <- GJRM::gamlss(list(numclaims ~ 1), family = "P", data = df2, offset = log(exposure))

res.check(P.freq)
summary(P.freq)
AIC(P.freq)
# 34943.67

# Requires loading mgcv for ldTweedie to fit Tweedie below
library(mgcv)
# Now can fit Tweedie for the claim amount just fine

TW.sev1 <- GJRM::gamlss(list(claimcst0 ~ 1, # mu
                               ~ 1, #sigma
                               ~ 1),  # nu
                          family = "TW", data = df2)


res.check(TW.sev1)
summary(TW.sev1)
AIC(TW.sev1)
# 113114.1

TW.sev2 <- GJRM::gamlss(list(claimcst0 ~ s(exposure) + area + gender + agecat, # mu
                               ~ s(veh_value) + agecat + s(exposure), #sigma
                               ~ gender + agecat + s(exposure)),  # nu
                          family = "TW", data = df2)


res.check(TW.sev2)
summary(TW.sev2)
AIC(TW.sev2)
# 111391.2


# Encountered ERROR here
P.TW.C0 <- GJRM::gjrm(list(numclaims ~ 1, # mu1 for freq
                           claimcst0 ~ s(exposure) + area + gender + agecat, # mu2 for sev
                           ~ s(veh_value) + agecat + s(exposure), #sigma2 ?
                           ~ gender + agecat + s(exposure), #nu2 ?
                           ~ 1 # theta for the copula ?
                      ),
                      data = df2, 
                      margins = c("P", "TW"),
                      offset1=log(exposure),
                      model = "B",
                      copula = "C0")

# Error in names(log.nu.2) <- "nu.2.star" : 
#   attempt to set an attribute on NULL



######
# Model on Truncated data: strictly positve claims amount

df2.trunc <- df2[df2$claimcst0 > 0,]

# This works just fine:
tP.IG.C0 <- gjrm(list(numclaims ~ 1,
                     claimcst0 ~ 1,
                     ~ 1,
                     ~ 1
                    ),
                data = df2.trunc, 
                margins = c("tP", "IG"),
                offset1=log(exposure),
                model = "B",
                copula = "C0")

res.check(tP.IG.C0)
summary(tP.IG.C0)
AIC(tP.IG.C0)
# 79321.95

