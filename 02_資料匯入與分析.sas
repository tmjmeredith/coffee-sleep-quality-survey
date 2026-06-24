

options nodate nonumber ls=120 ps=60;
%let PATH = D:\期末報告\coffee_sleep.csv;

/*==================================================================
  一、匯入資料
==================================================================*/
proc import datafile="&PATH" out=coffee dbms=csv replace;
  getnames=yes;
  guessingrows=max;
run;
 
/* 建立分析用衍生變數 */
data coffee;
  set coffee;
  /* 滿意度二分（>=70 為高滿意） */
  if not missing(sleepsat) then sat_bin = (sleepsat >= 70);
  /* 易驚醒二分（>=4 為易驚醒） */
  if not missing(wake) then wake_bin = (wake >= 4);
  /* 性別、輪班 0/1 旗標 */
  male  = (gender = "男");
  shift = (occ = "輪班制");
  /* 收入級距標籤 */
  length income_lab $8 occ_lab $14 last_lab $8;
  select(income);
    when(1) income_lab="3萬以下"; when(2) income_lab="3-5萬";
    when(3) income_lab="5-7萬";   when(4) income_lab="7-10萬";
    when(5) income_lab="10萬以上"; otherwise income_lab=" ";
  end;
  select(lasttime);
    when(1) last_lab="12時前"; when(2) last_lab="12-15時";
    when(3) last_lab="15-18時"; when(4) last_lab="18-21時";
    otherwise last_lab=" ";
  end;
run;
 
/*==================================================================
  二、描述性統計
==================================================================*/
title "表 數值變數描述統計";
proc means data=coffee n nmiss mean std min q1 median q3 max maxdec=2;
  var age cups sleephr sleepsat wake;
run;
 
title "表 類別變數次數分配";
proc freq data=coffee;
  tables gender occ income drink lasttime spend sleepdiff wake / missing;
run;
 
/* 飲用者複選題（種類、選擇因素、生理反應）之回應率 */
title "表 咖啡種類複選（飲用者）";
proc means data=coffee sum mean maxdec=3;
  where drink=1;
  var t_latte t_americano t_cappu t_mocha t_other;
run;
title "表 選擇因素複選（飲用者）";
proc means data=coffee sum mean maxdec=3;
  where drink=1;
  var f_taste f_conv f_price f_flavor f_caf f_origin f_brand;
run;
title "表 生理反應複選（飲用者）";
proc means data=coffee sum mean maxdec=3;
  where drink=1;
  var r_freq r_none r_excite r_palpit r_anx r_tremor r_appet;
run;
title;
 
/*==================================================================
  三、t 檢定
  H1：有飲用咖啡者之睡眠滿意度低於未飲用者
==================================================================*/
title "t 檢定：飲用咖啡與否 對 睡眠滿意度 / 睡眠時數";
proc ttest data=coffee;
  class drink;
  var sleepsat sleephr;
run;
 
title "t 檢定：性別 對 睡眠滿意度 / 睡眠時數";
proc ttest data=coffee;
  class gender;
  var sleepsat sleephr;
run;
title;
 
/*==================================================================
  四、卡方檢定
==================================================================*/
title "卡方檢定：飲用咖啡 × 睡眠滿意度高低";
proc freq data=coffee;
  tables drink*sat_bin / chisq expected cellchi2 nocol;
run;
 
title "卡方檢定：職業 × 睡眠滿意度高低";
proc freq data=coffee;
  tables occ*sat_bin / chisq nocol;
run;
 
title "卡方檢定：性別 × 是否飲用咖啡";
proc freq data=coffee;
  tables gender*drink / chisq nocol;
run;
 
title "卡方檢定：最後飲用時段 × 是否易驚醒（飲用者）";
proc freq data=coffee;
  where drink=1;
  tables last_lab*wake_bin / chisq nocol;
run;
title;
 
/*==================================================================
  五、變異數分析（ANOVA）+ Tukey 事後檢定
==================================================================*/
title "ANOVA：不同職業 之 睡眠滿意度差異";
proc glm data=coffee;
  class occ;
  model sleepsat = occ;
  means occ / tukey hovtest;
run; quit;
 
title "ANOVA：不同最後飲用時段 之 入睡難度差異（飲用者）";
proc glm data=coffee;
  where drink=1;
  class last_lab;
  model sleepdiff = last_lab;
  means last_lab / tukey;
run; quit;
title;
 
/*==================================================================
  六、簡單線性迴歸
==================================================================*/
title "簡單線性迴歸：每日杯數 -> 睡眠滿意度（飲用者）";
proc reg data=coffee;
  where drink=1;
  model sleepsat = cups / clb;
run; quit;
 
title "簡單線性迴歸：入睡難度 -> 睡眠滿意度（飲用者）";
proc reg data=coffee;
  where drink=1;
  model sleepsat = sleepdiff / clb;
run; quit;
 
title "簡單線性迴歸：睡眠時數 -> 睡眠滿意度（全體）";
proc reg data=coffee;
  model sleepsat = sleephr / clb;
run; quit;
title;
 
/*==================================================================
  七、決策樹（Decision Tree, PROC HPSPLIT）
  結局指標：睡眠滿意度是否達高滿意（sat_hi = sleepsat>=70）
  依 TRIPOD：建模(70%)＋內部驗證(30%)
==================================================================*/
/* 連續預測因子之缺失以中位數插補（單一插補） */
proc stdize data=coffee out=coffee_imp method=median reponly;
  var age income;
run;
 
data coffee_imp;
  set coffee_imp;
  /* 飲用者專屬變數對非飲用者以 0 帶入，使全體可進入模型 */
  cups0     = coalesce(cups, 0);
  lasttime0 = coalesce(lasttime, 0);
  sleepdiff0= coalesce(sleepdiff, 0);
  spend0    = coalesce(spend, 0);
  if not missing(sleepsat) then sat_hi = (sleepsat >= 70);
run;
 
title "決策樹：預測高睡眠滿意度（TRIPOD 建立＋內部驗證）";
proc hpsplit data=coffee_imp maxdepth=3 minleafsize=15 seed=42;
  class sat_hi male shift drink;
  model sat_hi(event='1') = age income cups0 lasttime0 sleepdiff0
                            spend0 male shift drink;
  partition fraction(validate=0.3 seed=42);
  prune costcomplexity;
run;
title;
 
/*==================================================================
  八、K-means 集群分析（PROC FASTCLUS）
  分群變數：年齡、睡眠時數、睡眠滿意度、易驚醒、每日杯數(0 帶入)
==================================================================*/
/* 先標準化（平均 0、標準差 1），避免量綱影響距離 */
proc stdize data=coffee_imp out=coffee_std method=std;
  var age sleephr sleepsat wake cups0;
run;
 
title "K-means 集群分析（k=3）";
proc fastclus data=coffee_std out=clusout maxclusters=3 maxiter=50 seed=42;
  var age sleephr sleepsat wake cups0;
run;
 
/* 將分群結果還原至原始量尺檢視各群輪廓 */
data clus_join;
  merge coffee_imp clusout(keep=cluster);
run;
 
title "各集群之變數平均（原始量尺）";
proc means data=clus_join mean maxdec=2;
  class cluster;
  var age sleephr sleepsat wake cups0 drink;
run;
title;
 
/* 全部完成 */
