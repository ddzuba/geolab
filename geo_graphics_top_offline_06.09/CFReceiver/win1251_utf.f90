SUBROUTINE win1251_utf(DEVICE,MyData,II) ! ������������ ������� � UTF-8
USE GEOSKANGLOBALS
IMPLICIT NONE
CHARACTER *50 DEVICE
INTEGER*4  J_START,N
CHARACTER*1::  MyData
CHARACTER(1) STR,STR1,STR2
INTEGER*4  II                           ! ����� ���T �����������
INTEGER*4   V
INTEGER*4  pos
DEVICE='                                                  '

II=0 

V = ICHAR(MyData)
IF(V.LT.128) THEN
    II=II+1; DEVICE(II:II)=CHAR(V);  GOTO 1
END IF
pos = (V-128)*3+1 ; ! ������� � ������� ��������
II=II+1; DEVICE(II:II)=(table(pos)) 
II=II+1; DEVICE(II:II)=(table(pos+1)); 
IF(table(pos+2).NE.CHAR(0)) then
II=II+1;    DEVICE(II:II)=(table(pos+2))     
end if
1 CONTINUE
end subroutine 
    
SUBROUTINE TABLE_() ! ������������ ������� � UTF-8
USE GEOSKANGLOBALS
IMPLICIT NONE

ALLOCATE(table(128*3))  
table(1)=CHAR(208); table(2)=CHAR(130); table(3)=CHAR(0)
table(4)=CHAR(208); table(5)=CHAR(131); table(6)=CHAR(0)
table(7)=CHAR(226); table(8)=CHAR(128); table(9)=CHAR(154)
table(10)=CHAR(209); table(11)=CHAR(147); table(12)=CHAR(0)
table(13)=CHAR(226); table(14)=CHAR(128); table(15)=CHAR(158)
table(16)=CHAR(226); table(17)=CHAR(128); table(18)=CHAR(166)
table(19)=CHAR(226); table(20)=CHAR(128); table(21)=CHAR(160)
table(22)=CHAR(226); table(23)=CHAR(128); table(24)=CHAR(161)
table(25)=CHAR(226); table(26)=CHAR(130); table(27)=CHAR(172)
table(28)=CHAR(226); table(29)=CHAR(128); table(30)=CHAR(176)
table(31)=CHAR(208); table(32)=CHAR(137); table(33)=CHAR(0)
table(34)=CHAR(226); table(35)=CHAR(128); table(36)=CHAR(185)
table(37)=CHAR(208); table(38)=CHAR(138); table(39)=CHAR(0)
table(40)=CHAR(208); table(41)=CHAR(140); table(42)=CHAR(0)
table(43)=CHAR(208); table(44)=CHAR(139); table(45)=CHAR(0)
table(46)=CHAR(208); table(47)=CHAR(143); table(48)=CHAR(0)
table(49)=CHAR(209); table(50)=CHAR(146); table(51)=CHAR(0)
table(52)=CHAR(226); table(53)=CHAR(128); table(54)=CHAR(152)
table(55)=CHAR(226); table(56)=CHAR(128); table(57)=CHAR(153)
table(58)=CHAR(226); table(59)=CHAR(128); table(60)=CHAR(156)
table(61)=CHAR(226); table(62)=CHAR(128); table(63)=CHAR(157)
table(64)=CHAR(226); table(65)=CHAR(128); table(66)=CHAR(162)
table(67)=CHAR(226); table(68)=CHAR(128); table(69)=CHAR(147)
table(70)=CHAR(226); table(71)=CHAR(128); table(72)=CHAR(148)
table(73)=CHAR(239); table(74)=CHAR(191); table(75)=CHAR(189)
table(76)=CHAR(226); table(77)=CHAR(132); table(78)=CHAR(162)
table(79)=CHAR(209); table(80)=CHAR(153); table(81)=CHAR(0)
table(82)=CHAR(226); table(83)=CHAR(128); table(84)=CHAR(186)
table(85)=CHAR(209); table(86)=CHAR(154); table(87)=CHAR(0)
table(88)=CHAR(209); table(89)=CHAR(156); table(90)=CHAR(0)
table(91)=CHAR(209); table(92)=CHAR(155); table(93)=CHAR(0)
table(94)=CHAR(209); table(95)=CHAR(159); table(96)=CHAR(0)
table(97)=CHAR(194); table(98)=CHAR(160); table(99)=CHAR(0)
table(100)=CHAR(208); table(101)=CHAR(142); table(102)=CHAR(0)
table(103)=CHAR(209); table(104)=CHAR(158); table(105)=CHAR(0)
table(106)=CHAR(208); table(107)=CHAR(136); table(108)=CHAR(0)
table(109)=CHAR(194); table(110)=CHAR(164); table(111)=CHAR(0)
table(112)=CHAR(210); table(113)=CHAR(144); table(114)=CHAR(0)
table(115)=CHAR(194); table(116)=CHAR(166); table(117)=CHAR(0)
table(118)=CHAR(194); table(119)=CHAR(167); table(120)=CHAR(0)
table(121)=CHAR(208); table(122)=CHAR(129); table(123)=CHAR(0)
table(124)=CHAR(194); table(125)=CHAR(169); table(126)=CHAR(0)
table(127)=CHAR(208); table(128)=CHAR(132); table(129)=CHAR(0)
table(130)=CHAR(194); table(131)=CHAR(171); table(132)=CHAR(0)
table(133)=CHAR(194); table(134)=CHAR(172); table(135)=CHAR(0)
table(136)=CHAR(194); table(137)=CHAR(173); table(138)=CHAR(0)
table(139)=CHAR(194); table(140)=CHAR(174); table(141)=CHAR(0)
table(142)=CHAR(208); table(143)=CHAR(135); table(144)=CHAR(0)
table(145)=CHAR(194); table(146)=CHAR(176); table(147)=CHAR(0)
table(148)=CHAR(194); table(149)=CHAR(177); table(150)=CHAR(0)
table(151)=CHAR(208); table(152)=CHAR(134); table(153)=CHAR(0)
table(154)=CHAR(209); table(155)=CHAR(150); table(156)=CHAR(0)
table(157)=CHAR(210); table(158)=CHAR(145); table(159)=CHAR(0)
table(160)=CHAR(194); table(161)=CHAR(181); table(162)=CHAR(0)
table(163)=CHAR(194); table(164)=CHAR(182); table(165)=CHAR(0)
table(166)=CHAR(194); table(167)=CHAR(183); table(168)=CHAR(0)
table(169)=CHAR(209); table(170)=CHAR(145); table(171)=CHAR(0)
table(172)=CHAR(226); table(173)=CHAR(132); table(174)=CHAR(150)
table(175)=CHAR(209); table(176)=CHAR(148); table(177)=CHAR(0)
table(178)=CHAR(194); table(179)=CHAR(187); table(180)=CHAR(0)
table(181)=CHAR(209); table(182)=CHAR(152); table(183)=CHAR(0)
table(184)=CHAR(208); table(185)=CHAR(133); table(186)=CHAR(0)
table(187)=CHAR(209); table(188)=CHAR(149); table(189)=CHAR(0)
table(190)=CHAR(209); table(191)=CHAR(151); table(192)=CHAR(0)
table(193)=CHAR(208); table(194)=CHAR(144); table(195)=CHAR(0)
table(196)=CHAR(208); table(197)=CHAR(145); table(198)=CHAR(0)
table(199)=CHAR(208); table(200)=CHAR(146); table(201)=CHAR(0)
table(202)=CHAR(208); table(203)=CHAR(147); table(204)=CHAR(0)
table(205)=CHAR(208); table(206)=CHAR(148); table(207)=CHAR(0)
table(208)=CHAR(208); table(209)=CHAR(149); table(210)=CHAR(0)
table(211)=CHAR(208); table(212)=CHAR(150); table(213)=CHAR(0)
table(214)=CHAR(208); table(215)=CHAR(151); table(216)=CHAR(0)
table(217)=CHAR(208); table(218)=CHAR(152); table(219)=CHAR(0)
table(220)=CHAR(208); table(221)=CHAR(153); table(222)=CHAR(0)
table(223)=CHAR(208); table(224)=CHAR(154); table(225)=CHAR(0)
table(226)=CHAR(208); table(227)=CHAR(155); table(228)=CHAR(0)
table(229)=CHAR(208); table(230)=CHAR(156); table(231)=CHAR(0)
table(232)=CHAR(208); table(233)=CHAR(157); table(234)=CHAR(0)
table(235)=CHAR(208); table(236)=CHAR(158); table(237)=CHAR(0)
table(238)=CHAR(208); table(239)=CHAR(159); table(240)=CHAR(0)
table(241)=CHAR(208); table(242)=CHAR(160); table(243)=CHAR(0)
table(244)=CHAR(208); table(245)=CHAR(161); table(246)=CHAR(0)
table(247)=CHAR(208); table(248)=CHAR(162); table(249)=CHAR(0)
table(250)=CHAR(208); table(251)=CHAR(163); table(252)=CHAR(0)
table(253)=CHAR(208); table(254)=CHAR(164); table(255)=CHAR(0)
table(256)=CHAR(208); table(257)=CHAR(165); table(258)=CHAR(0)
table(259)=CHAR(208); table(260)=CHAR(166); table(261)=CHAR(0)
table(262)=CHAR(208); table(263)=CHAR(167); table(264)=CHAR(0)
table(265)=CHAR(208); table(266)=CHAR(168); table(267)=CHAR(0)
table(268)=CHAR(208); table(269)=CHAR(169); table(270)=CHAR(0)
table(271)=CHAR(208); table(272)=CHAR(170); table(273)=CHAR(0)
table(274)=CHAR(208); table(275)=CHAR(171); table(276)=CHAR(0)
table(277)=CHAR(208); table(278)=CHAR(172); table(279)=CHAR(0)
table(280)=CHAR(208); table(281)=CHAR(173); table(282)=CHAR(0)
table(283)=CHAR(208); table(284)=CHAR(174); table(285)=CHAR(0)
table(286)=CHAR(208); table(287)=CHAR(175); table(288)=CHAR(0)
table(289)=CHAR(208); table(290)=CHAR(176); table(291)=CHAR(0)
table(292)=CHAR(208); table(293)=CHAR(177); table(294)=CHAR(0)
table(295)=CHAR(208); table(296)=CHAR(178); table(297)=CHAR(0)
table(298)=CHAR(208); table(299)=CHAR(179); table(300)=CHAR(0)
table(301)=CHAR(208); table(302)=CHAR(180); table(303)=CHAR(0)
table(304)=CHAR(208); table(305)=CHAR(181); table(306)=CHAR(0)
table(307)=CHAR(208); table(308)=CHAR(182); table(309)=CHAR(0)
table(310)=CHAR(208); table(311)=CHAR(183); table(312)=CHAR(0)
table(313)=CHAR(208); table(314)=CHAR(184); table(315)=CHAR(0)
table(316)=CHAR(208); table(317)=CHAR(185); table(318)=CHAR(0)
table(319)=CHAR(208); table(320)=CHAR(186); table(321)=CHAR(0)
table(322)=CHAR(208); table(323)=CHAR(187); table(324)=CHAR(0)
table(325)=CHAR(208); table(326)=CHAR(188); table(327)=CHAR(0)
table(328)=CHAR(208); table(329)=CHAR(189); table(330)=CHAR(0)
table(331)=CHAR(208); table(332)=CHAR(190); table(333)=CHAR(0)
table(334)=CHAR(208); table(335)=CHAR(191); table(336)=CHAR(0)
table(337)=CHAR(209); table(338)=CHAR(128); table(339)=CHAR(0)
table(340)=CHAR(209); table(341)=CHAR(129); table(342)=CHAR(0)
table(343)=CHAR(209); table(344)=CHAR(130); table(345)=CHAR(0)
table(346)=CHAR(209); table(347)=CHAR(131); table(348)=CHAR(0)
table(349)=CHAR(209); table(350)=CHAR(132); table(351)=CHAR(0)
table(352)=CHAR(209); table(353)=CHAR(133); table(354)=CHAR(0)
table(355)=CHAR(209); table(356)=CHAR(134); table(357)=CHAR(0)
table(358)=CHAR(209); table(359)=CHAR(135); table(360)=CHAR(0)
table(361)=CHAR(209); table(362)=CHAR(136); table(363)=CHAR(0)
table(364)=CHAR(209); table(365)=CHAR(137); table(366)=CHAR(0)
table(367)=CHAR(209); table(368)=CHAR(138); table(369)=CHAR(0)
table(370)=CHAR(209); table(371)=CHAR(139); table(372)=CHAR(0)
table(373)=CHAR(209); table(374)=CHAR(140); table(375)=CHAR(0)
table(376)=CHAR(209); table(377)=CHAR(141); table(378)=CHAR(0)
table(379)=CHAR(209); table(380)=CHAR(142); table(381)=CHAR(0)
table(382)=CHAR(209); table(383)=CHAR(143); table(384)=CHAR(0)
end subroutine     