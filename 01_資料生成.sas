/**********************************************************************
 進階 SAS 程式設計 期末報告
 程式 01：以 SAS 亂數函數「同分配隨機生成」咖啡飲用與睡眠資料
 說明：本程式依原始問卷 (n=43) 之邊際分配與假設之關聯結構，
       模擬產生 277 筆資料，與原始樣本合併為 N=320。
       ※ 為使報告數據可完全重現，正式分析請以隨附之
         coffee_sleep.csv（程式 02 匯入）為準；本程式示範
         題目所要求之「以 SAS 亂數函數產生資料」之方法。

**********************************************************************/
options nodate nonumber;
%let SEED = 20260617;

data sim;
  call streaminit(&SEED);
  length gender $2 occ $12;
  do id = 44 to 320;                      /* 與原始 43 筆銜接 */
     /* 性別：男 0.55 */
    gender = ifc(rand("uniform") < 0.55, "男", "女");

    /* 撟湧翩嚗 /* 年齡：右偏（gamma），集中於 20-30，截尾 18-72 */
    age = round(20 + rand("gamma", 2.0) * 4.2);
    if age < 18 then age = 18; if age > 72 then age = 72;

    /* 職業：依年齡條件 */
    u = rand("uniform");
    if age >= 62 then do;
      if u<0.70 then occ="退休"; else if u<0.90 then occ="一般上班族"; else occ="輪班制";
    end;
    else if age <= 23 then do;
      if u<0.78 then occ="學生"; else if u<0.93 then occ="一般上班族"; else occ="輪班制";
    end;
    else do;
      if u<0.50 then occ="一般上班族"; else if u<0.68 then occ="輪班制";
      else if u<0.86 then occ="自由接案者"; else if u<0.98 then occ="輪班制"; else occ="退休";
    end;

      /* 月收入級距 1-5（.=不便透露/未填） */
    v = rand("uniform");
    if occ="學生"        then income = ifn(v<0.70,1, ifn(v<0.85,2,.));
    else if occ="退休"   then income = ifn(v<0.40,1, ifn(v<0.70,2, ifn(v<0.85,3,.)));
    else do;
      if v<0.18 then income=1; else if v<0.52 then income=2; else if v<0.74 then income=3;
      else if v<0.86 then income=4; else if v<0.92 then income=5; else income=.;
    end;

   /* 潛在睡眠體質 Z（越大越好），輪班/高齡較差 */
    shiftpen = ifn(occ="輪班制", -0.7, 0);
    Z = rand("normal") + shiftpen - 0.012*(age-28);

    /* 是否飲用咖啡 0.62 */
    drink = (rand("uniform") < 0.62);

    cups=.; lasttime=.; sleepdiff=.; spend=.;
    if drink=1 then do;
      cups = 0.5 + rand("gamma", 2.0)*0.7;
      cups = round(cups*2)/2; if cups<0.5 then cups=0.5; if cups>5 then cups=5;
       /* 最後飲用時段 1-4，杯數多者偏晚 */
      lt = rand("uniform");
      if lt<0.18 then lasttime=1; else if lt<0.50 then lasttime=2;
      else if lt<0.80 then lasttime=3; else lasttime=4;
      sleepdiff = round(1 + 0.55*cups + 0.45*(lasttime-1) + rand("normal")*0.8);
      if sleepdiff<1 then sleepdiff=1; if sleepdiff>5 then sleepdiff=5;
      spend = 1 + (cups>1.5) + (cups>3) + (rand("uniform")<0.15);
      if spend>3 then spend=3;
    end;

    /* 睡眠結果（全體） */
    cups_eff = coalesce(cups,0);
    late_eff = coalesce(lasttime,1)-1;
    sleephr = round(7.3 - 0.18*cups_eff - 0.18*late_eff + 0.55*Z + rand("normal")*0.7, 0.1);
    if sleephr<3.5 then sleephr=3.5; if sleephr>11 then sleephr=11;

    sd_eff = ifn(missing(sleepdiff),0,sleepdiff-1);
    drinkpen = ifn(drink=1, -3, 0);
    sleepsat = round(70 + 11*Z + 3.5*(sleephr-7) - 4.2*sd_eff - 1.6*cups_eff + drinkpen + rand("normal")*7);
    if sleepsat<0 then sleepsat=0; if sleepsat>100 then sleepsat=100;

    wake = round(3 - 1.15*Z + 0.18*cups_eff + 0.12*late_eff + rand("normal")*0.7);
    if wake<1 then wake=1; if wake>5 then wake=5;

    output;
  end;
  drop u v lt shiftpen Z cups_eff late_eff sd_eff drinkpen;
run;

/* 隨機注入非跳答遺失值（MCAR），確保 >=10 個 */
data sim_miss; set sim;
  call streaminit(&SEED + 7);
  if rand("uniform")<0.02 then age=.;
  if rand("uniform")<0.02 then sleepsat=.;
  if rand("uniform")<0.015 then sleephr=.;
  if rand("uniform")<0.012 then wake=.;
  if drink=1 and rand("uniform")<0.02 then spend=.;
run;

proc means data=sim_miss n nmiss mean std min max maxdec=2;
  var age cups sleephr sleepsat wake;
run;
title "模擬資料之飲用習慣分配"; proc freq data=sim_miss; tables drink gender occ; run; title;
