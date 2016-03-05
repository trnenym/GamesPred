# GamesPred
Predicting success of video games based on descriptive data

Work in progress.

To my best knowledge, there is no universal tool for analysis of video games with the aim of providing an estimation of game's success. Previous studies focused on retail sales which are now irrelevant for indie studios relying on digital distribution via Steam. The goal of this project is to offer an intuitive way of finding how successful a game will be based on basic information (see below).

Data so far:
3,021 games released on Steam August 2012 - July 2015

Average number of players in first two months as a measure of success (source: SteamCharts)

Attributes:

1                    id
2                  Name
3           Description
4             Developer
5             Publisher
6           RequiredAge
7            Controller
8               Windows
9                   Mac
10                Linux
11         Singleplayer
12          Multiplayer
13                 Coop
14            LocalCoop
15    SteamAchievements
16    SteamTradingCards
17        SteamWorkshop
18          Screenshots
19             Trailers
20                 Year
21                Month
22                  Day
23              Weekday
24            YearMonth
25          LaunchPrice
26           PriceGroup
27              English
28               French
29               German
30              Italian
31             Japanese
32               Polish
33           Portuguese
34              Russian
35              Spanish
36                  RPG
37             Strategy
38            Adventure
39               Action
40           Simulation
41               Racing
42               Casual
43               Sports
44 MassivelyMultiplayer
45            Education
46                Indie
47           NameLength
48    DescriptionLength
49             UserTags
50              Players

Results so far:
correlation 0.75 using Random Forest (see results)