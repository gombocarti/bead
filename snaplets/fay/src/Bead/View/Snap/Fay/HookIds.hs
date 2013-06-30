module Bead.View.Snap.Fay.HookIds where

data HookId = HookId { hookId :: String }

createCourseForm = HookId "create-course-form"
createGroupForm = HookId "create-group-form"
evaulationTypeSelection = HookId "eval-type-selection"
evaulationTypeValue = HookId "eval-type-value"
evalTypeSelectionDiv = HookId "eval-type-selection-div"

startDateDivId = HookId "start-date-div"
endDateDivId = HookId "end-date-div"

assignmentStartField = HookId "asg-start"
assignmentEndField = HookId "asg-end"

evaulationPercentageDiv = HookId "evaulation-pct-div"
evaulationResultField = HookId "evaulation-result"

data HookClass = HookClass { hookClass :: String }

datePickerClass = HookClass "datepicker"
hourSpinnerClass = HookClass "hourspinner"
minuteSpinnerClass = HookClass "minutespinner"

data LoginField = LoginField { lcFieldName :: String }

loginUsername = LoginField "login"
loginPassword = LoginField "password"

data RegistrationComp = RegComp { rFieldName :: String }

regFullName = RegComp "reg_full_name"
regEmailAddress = RegComp "reg_email_address"

data FormId = FormId { rFormId :: String }

loginForm = FormId "login-form"
regForm = FormId "reg-form"

