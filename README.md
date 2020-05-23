<h1 style="font-weight:normal" align="center">
  &nbsp;BAAC - ENSAE final school project
</h1>

# Summary
BAAC stands for "Bulletin d’Analyse des Accidents de la Circulation" which is a yearly updated database that records all circulation accidents with their characteristics and the health of the people involved in the accident. They can be either unscathed, slightly injured, seriousily injured (hospitalization needed) or dead.

# Objective
As a project to validate our data scientist certificate at ENSAE Paris, we decided to use this database to create a model that could be used by 112 / emergency call center to forecast the materiels that need to be sent at the accident site, depending on the injury levels of the people involved in the accident.

# Method
We use some variables provided in BAAC like weather, luminosity, vehicle category, type of road, role (conductor, passenger, pedestrian) ... to predict wether the casualty needs high level of care or not. It's critical for a seriousily injured person to be correctly predicted in order to send the right material on site. On the contrary, mixing between unscathed and lightly injured is not as important.

Due to these considerations, we decided to create a cost metric based on the cost matrix below:

| True/Pred  | Unscathed | Slightly injured | Seriousily injured | Dead |
| ------------- | ------------- | ------------- | ------------- | ------------- |
| Unscathed  | 0  | 1  | 5  | 1  |
| Slightly injured  | 5  | 0  | 1  | 5  |
| Seriousily injured  | 10  | 5  | 0  | 10  |
| Dead  | 1  | 5  | 5  | 0  |



# Models tested

- Random Forest
- Neural Networks
- Multinomial regression


