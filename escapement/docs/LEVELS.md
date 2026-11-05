# Escapement 机关房间：详细关卡设计与开发提示词

> 状态：设计稿，**数字都是初稿**，还没有经过搜索验证，也没有人试玩过。每个房间的提示词都要求先用求解器验证，再调参数。
> 用法：先读第 1 到 3 节，在第 3 节做完决定，再按第 6 节的顺序，把提示词逐条交给 Claude Code 会话执行。每条提示词都可以单独使用，只需要先贴上第 5 节的「通用上下文」。

---

## 1. 从代码里得出的硬约束

下面这些数字决定了几何能怎么画，全部来自 `src/Game/Types.hs` 的 `defaultConfig` 和 `src/Game/Logic.hs`。

| 量 | 值 | 设计含义 |
|---|---|---|
| 逻辑帧率 | 60 tick/s | 下文的“帧”都是 tick |
| 玩家速度 | 3.25 px/tick（195 px/s） | 各状态相同 |
| 玩家半径 / 齿轮半径 | 13 / 28 | 判定距离 = 13 + 28 − 3 = **38 px** |
| 债务上限 | 180 帧（3 秒） | 一次按住最多走 **585 px** |
| 走廊“堵死”宽度 | 宽度 < 约 102 px | 齿轮停在走廊中线附近时过不去，所以停的位置很重要 |
| 门 | 厚 14 px，开口通常 70 px | 门只受时钟控制：冻结时保持原状，还债时以 2 倍速开关 |
| 还债 | 债务 D 用 D 帧还清，期间世界走 2D | 还清时齿轮正好在“幽灵”位置 |
| 场地 | x ∈ [−640, 640]，y ∈ [−290, 360] | 左上角 (−576, 330) 一带刻着楼层名，不要放机关 |
| 销钉拾取 | 距离 ≤ 28 px | |
| 评价 | `gBorrowed ≤ lvlPar` 时显示 PAID IN FULL | 第 4 节会把它扩成三档 |

**粗算公式**：齿轮速度 = 路线长 ÷ `routeOneWay`。只要它大于约 2 倍玩家速度（6.5 px/tick），玩家就不可能靠走位躲开，必须用刹车。

---

## 2. 核心难题：让顺序影响借款

样板房间里，6 种顺序的用时差别明显（19 到 23 秒），借款却几乎一样（7.5 到 7.8 秒）。原因是三个翼互相独立：每个翼的价格固定，先拿哪个，只改变总和的排列方式，不改变总和。

下面 4 个杠杆能让顺序真正影响借款。房间设计至少要用到其中一个。

| 杠杆 | 做法 | 需要新规则吗 |
|---|---|---|
| **L1 一次按住拿两个** | 一个齿轮挡着两个翼。在对的位置停住它，一次冻结可以连拿两个销钉，但只适用于特定顺序 | 不需要 |
| **L2 避难所的位置** | 安全区（能原地还债的地方）只在某些顺序的路线上。走反方向，就得在开放区边走边还，或者多冻结一次 | 不需要 |
| **L3 门的相位** | 门的开关只看时钟，所以到达门口时的相位取决于之前借了多少、走了多久。顺序对了，门正好开着，不用借 | 不需要 |
| **L4 销钉卡死齿轮** | 拔下某个销钉，会让一个指定齿轮永久停下或改变路线。先拔哪个，就先拆掉哪道障碍 | **需要**：新增一个字段和一条规则 |

**判定标准（每个非教学房间都要满足）**：用 P1 求解器分别算出各顺序的最小借款。最好顺序与最差顺序之间至少差 **25% 或 0.5 秒**，取两者中较大的一个，而且最好顺序不能一眼就看出来。

---

## 3. 需要你拍板的决定

| # | 问题 | 我的建议 |
|---|---|---|
| D1 | 要不要加 L4“销钉卡死齿轮”？ | **先不加。** 先用 P1 求解器量化 R2 到 R4 的顺序差。如果达不到第 2 节的标准，再执行提示词 P1b 加上它 |
| D2 | 评价分几档？ | 三档：`PAID IN FULL`（≤ par）、`IN GOOD STANDING`（≤ 1.5 × par）、`IN ARREARS`（其余）。par 取求解器最优值 × 1.15 后取整 |
| D3 | 原来的 5 层关卡怎么处理？ | 从主流程里拿掉，代码保留，可以用 `--classic` 进入，方便对比试玩 |
| D4 | 做多少个房间？ | 7 个主线房间，外加 1 个可选挑战房间（R8） |
| D5 | 房间之间有没有选关 / 地图？ | 没有，线性推进。开放性只放在房间内部，控制工作量 |

---

## 4. 房间总表

| # | 名字 | 新想法 | 顺序杠杆 | 销钉数 | 刹车必需 |
|---|---|---|---|---|---|
| R1 | THE PIN ROOM | 销钉，以及“先后会影响借款” | L2 | 2 | 只有一个销钉需要 |
| R2 | THE CROSSING | 齿轮停在哪里，决定哪几条路通 | L1 | 3 | 是 |
| R3 | THE BELL DOORS | 时钟门：等，还是按住 | L3 | 3 | 是 |
| R4 | THE COUNTING HOUSE | 还债锁定和落点幽灵 | L2 + 方向 | 3 | 是 |
| R5 | THE GALLERY RING | 走廊里的避难凹室 | L2 | 3 | 是 |
| R6 | THE ESCAPE WHEEL | 综合复习（重新调整现有样板房） | L1 + L3 | 3 | 是 |
| R7 | THE MAINSPRING | 终局：同时用上所有机制 | 全部 | 4 | 是 |
| R8 (可选) | THE LEDGER | 挑战：par 很紧，关掉幽灵 | 全部 | 3 | 是 |

整体节奏：R1 和 R2 很快（约 20 秒），R3 到 R5 中等（约 30 秒），R6 和 R7 较长（约 45 秒）。全程完美通关大约 4 分钟，真人游玩估计 15 到 25 分钟。

---

## 5. 各房间详细设计

坐标都是**初稿**，格式是 `Rect 左 下 右 上`，齿轮写作 `Route [起点, 终点] oneWay phase`。

### R1 THE PIN ROOM（教学）

**目标**：只教两件事：“拔完所有销钉，出口才会开”和“按住空格”。另外给玩家第一次尝到“顺序有代价”。

```
 [ 起始房 ]--门--| 竖缝(快齿轮) |--门--[      工坊 (销钉 B)      EXIT ]
 [ 南侧凹龛: 销钉 A ]
```

- 几何：直接复用 Undercroft。起始房 `Rect (-600) (-140) (-400) 200`，竖缝齿轮 `Route [(-340,120),(-340,20)] 15 0`，工坊 `Rect (-280) (-140) 500 200`。
- 新增南侧凹龛 `Rect (-560) (-250) (-440) (-140)`，销钉 A 放在 `(-500,-200)`，不用刹车就能拿到。
- 销钉 B 放在 `(200,-60)`（工坊里），出口 `Rect 400 36 468 104`（工坊里）。
- **顺序差**：
  - 先 A 后 B：只需穿过竖缝一次，冻结 1 次。
  - 先 B 后 A：要来回穿过竖缝三次，冻结 3 次。
- par：先 A 的最优借款 × 1.15，预计约 0.7 秒。
- 例外：允许 A 不用刹车。B 和出口必须要刹车。
- 玩家身边提示：`PULL EVERY PIN`（起始房，Normal）、`HOLD SPACE`（竖缝前）、`KEEP HOLDING. WALK AROUND IT`（Frozen）、`LET GO OF SPACE`（工坊，Frozen）。
- 工头笔记：
  - `MIDNIGHT. ST. ORRIN'S TOWER. THE GREAT CLOCK HAS SLIPPED.`
  - `EVERY ROOM IS HELD SHUT BY PINS. PULL THEM ALL AND THE WAY UP OPENS.`
  - `THE FOREMAN'S BRAKE IS IN YOUR HAND. HOLD IT AND THE TOWER STANDS STILL.`
- 失败提示：`TOO FAST TO DODGE. HOLD SPACE BEFORE YOU REACH IT.` / `TAKE THE NEAR PIN FIRST. EVERY CROSSING COSTS.`

### R2 THE CROSSING（停在哪里）

**目标**：一个横向齿轮横穿十字路口。它停在哪一侧，哪一侧就被堵住，另一侧和中央就通了。

```
              [  北厅 (销钉 N, 避难, EXIT)  ]
                         门
 [西厅 销钉W]==西臂=====[ 路口 ]=====东臂==[东厅 销钉E]
                         门
              [      南厅 (起点)      ]
```

- 路口 `Rect (-60) (-60) 60 60`；西臂 `Rect (-420) (-45) (-60) 45`、东臂 `Rect 60 (-45) 420 45`，宽 90，停在臂里就会堵死。
- 西厅 `Rect (-600) (-120) (-420) 120`，东厅 `Rect 420 (-120) 600 120`。
- 南门 `Rect (-35) (-74) 35 (-60)` 和南厅 `Rect (-200) (-270) 200 (-74)`；北门 `Rect (-35) 60 35 74` 和北厅 `Rect (-160) 74 160 230`。
- 齿轮：`Route [(-400,0),(400,0)] 100 0`，速度 8 px/tick，玩家跑不过。
- 销钉：W `(-540,0)`、E `(540,0)`、N `(120,190)`。出口在北厅北侧凹口 `Rect (-34) 230 34 290`。
- **顺序差（L1 + L2）**：
  - 东、西两厅之间的距离约 900 px，超过 585 px，一次按住走不完，中途必须在北厅避难还债。
  - 所以 N 适合放在中间。最后一个销钉在东或西时，还要再穿回北厅一次。
  - 抓齿轮时机好，一次按住可以“南 → 东”一口气走完；时机差，就得分两次。
- 只有 N 能不用刹车拿到（从南到北只需 41 帧，可以卡空档）。W、E 都必须用刹车。
- 提示：`CATCH IT AWAY FROM WHERE YOU ARE GOING`（南厅，Normal）、`A STOPPED WHEEL STILL CUTS`（Frozen）。
- 工头笔记：
  - `THE BRAKE STOPS THE CLOCK. IT DOES NOT STOP THE TEETH.`
  - `A WHEEL STOPS WHEREVER YOU CATCH IT, AND IT CUTS WHERE IT STOPS.`
  - `CHOOSE WHERE IT WAITS, AND IT WILL WAIT THERE FOR YOU.`
- 失败提示：`CATCH THE WHEEL IN THE ARM YOU ARE NOT USING.` / `THE NORTH HALL IS SAFE. PAY THERE.`

### R3 THE BELL DOORS（时钟门：等，还是按住）

**目标**：门只听时钟的。站在安全的地方等，不花一分；在危险的地方，只能按住刹车把门留在开着的状态。

```
           [ 北库: 两道反相门 + 中间快齿轮 → 销钉 N ]
                         |
 [西库 销钉W]--门W--[ 安全大厅 (起点, EXIT) ]--东廊(长齿轮扫过)--门E--[东库 销钉E]
```

- 安全大厅约 `Rect (-150) (-120) 150 120`，所有齿轮都碰不到这里，是固定的避难所。
- **西库**：门 W `Gate period 180 open 60`。门外是安全前厅，可以免费等它开。但西库里有一个快齿轮守着销钉，所以必须按住。
- **东库**：东廊长约 400 px，被齿轮 `oneWay 50` 来回扫，站在里面等就会死。门 E `Gate period 300 open 40`。要在门开的时候按住，再走到门里。
- **北库**：两道相位相反的门（period 120，open 40，phase 相差 60），中间有快齿轮，沿用样板房南翼的想法。
- **顺序差（L3）**：三道门的周期是 180、300、120，最小公倍数是 1800。从大厅出发时的时钟相位，取决于前面借了多少、走了多久，所以有的顺序到东门时正好开着（可以少借），有的顺序要多按住约 1.5 秒。设计时用求解器挑选相位，让最好的顺序不是“从近到远”。
- 提示：`DOORS ANSWER TO THE CLOCK`（大厅，Normal）、`HOLD IT OPEN`（门旁，Frozen）。
- 工头笔记：
  - `EVERY DOOR IN THIS TOWER ANSWERS TO THE CLOCK, AND ONLY TO THE CLOCK.`
  - `STOP THE CLOCK AND A DOOR STAYS JUST AS IT IS.`
  - `WAITING COSTS NOTHING, WHERE IT IS SAFE TO WAIT.`
- 失败提示：`BRAKE WHILE A DOOR IS OPEN AND IT STAYS OPEN.` / `WAIT IN THE HALL. THE DOORS COME TO YOU.`

### R4 THE COUNTING HOUSE（还债锁定与幽灵）

**目标**：刹车是一笔贷款。松手后时钟以 2 倍速追回来，这段时间刹车锁死。玩家要学会看落点幽灵再决定什么时候松手。

```
     [格A 销钉] ==竖井2== [格B 销钉]
        ||                   ||
      竖井1                竖井3
        ||                   ||
     [格S 起点 EXIT] ==竖井4== [格C 销钉]
```

- 4 个小格围成一圈，用 4 条竖井（复用 `shaftFloors` 的模式）连接。每条竖井有一个快齿轮 `oneWay 16`，各自的 phase 不同。
- 格子是安全的避难所，但很小（约 60 × 120），只能站着还债。
- **顺序差（方向）**：只有两种走法：顺时针 S → A → B → C → S，或逆时针反过来。phase 要设计成：
  - 一个方向上，每次在格子里还清时，下一个齿轮的幽灵正好落在竖井端点，可以立刻过去。
  - 另一个方向上，幽灵落在竖井中线，会堵住路。要么等一整个周期（不花借款，但花时间），要么多借一段，把齿轮“推”到端点。
  - 再加一条捷径（S 到 B 的对角竖井，齿轮更慢），让第三、四种顺序存在，这样才满足至少 2 种顺序都能通关。
- 第一次出现“一次按住过不了两条竖井”：在 Probe 里加一个测试证明这一点，参考 Counting Room 的做法。
- 提示：`LOCKED UNTIL PAID. GHOSTS SHOW WHERE THE WHEELS LAND`（Repaying）、`LET GO AS SOON AS YOU ARE THROUGH`（Frozen）。
- 工头笔记：
  - `THE BRAKE IS NOT A GIFT. IT IS A LOAN.`
  - `LET GO AND THE CLOCK RUNS DOUBLE UNTIL IT HAS IT ALL BACK.`
  - `UNTIL THEN THE BRAKE WILL NOT ANSWER. THE GHOSTS SHOW WHAT YOU OWE.`
- 失败提示：`NO SINGLE HOLD CROSSES TWO SHAFTS.` / `WATCH WHERE THE GHOST LANDS BEFORE YOU LET GO.`

### R5 THE GALLERY RING（避难凹室）

**目标**：在长走廊里超不过齿轮，只能躲进凹室，让它过去。

```
  ┌─────────── 北廊 (齿轮沿 U 形来回) ───────────┐
  │凹1                                       凹2│
 西廊                [ 中央石块 ]               东廊
  │                                            │
 [起点 / EXIT]                         [东端 销钉E]
  西端 销钉W 在西廊底部凹口                北廊中部 销钉N（在凹室对面）
```

- 一条 U 形走廊，宽 92 px，上面跑一个齿轮。它的 Route 有三段折线：`[(-560,-200),(-560,250),(560,250),(560,-200)]`，`oneWay ≈ 160`，在 U 形上来回。
- 凹室：凹 1 在西北角附近，凹 2 在北廊东段。**故意不对称**：凹 1 离西端近，凹 2 离东端远。
- **顺序差（L2）**：
  - 先东后西：每段都有凹室在半途接住你，债可以在凹室里还。
  - 先西后东：东段没有合适的凹室，只能边走边还，或者多冻结一次。
- 提示：只用失败提示，不再给玩家身边提示（从这一关开始收掉引导）。
- 工头笔记：
  - `THE FERRY THAT SHOULD HAVE LEFT. THE DOCTOR AT THE WRONG DOOR.`
  - `THREE SECONDS HERE, THREE THERE. THE TOWN NEVER NOTICED.`
  - `THIRTY YEARS I BORROWED, AND TONIGHT IT ALL FELL DUE.`
- 失败提示：`NO ONE PASSES A WHEEL IN THE GALLERY. LET IT PASS YOU.` / `STOP IT BEYOND AN ALCOVE, THEN LET GO INSIDE.`

### R6 THE ESCAPE WHEEL（综合复习，调整现有样板房）

- 保留现有 `escapeWheel` 的骨架（中央大厅、西竖缝、东长廊、南双门）。
- **要修的问题**：现在 3 个翼完全独立，顺序不影响借款。改法：
  - **L1**：东长廊的齿轮在大厅东口也能挡住南翼入口（把南翼入口移到大厅东南角）。这样只要在正确位置停住它，一次按住可以“东 → 南”或“南 → 东”。
  - **L3**：南门的周期改成和东齿轮周期互质，从东翼回来时的相位决定南门开不开。
- 用 P1 求解器重算，par 按 D2 的规则重设。
- 工头笔记保持现有文字，第 4 行改为 `EVERY DOOR, EVERY WHEEL, ONE CLOCK.`

### R7 THE MAINSPRING（终局）

**目标**：全部机制同时出现，4 个销钉，出口就是主发条。

- 布局：左半边借 R2 的十字（停住位置），右半边借 R5 的凹室走廊，顶部一道 R3 式时钟门，底部借 R4 的两条竖井。
- 4 个销钉分在四个区，出口在中央主发条，最后一步要穿过一个只能边走边还的开放区（沿用原第 5 层的“边走边还”）。
- **顺序差**：每个区的价格取决于进入时的相位和手上剩多少债。求解器要证明：至少 3 种顺序可以通关，最好与最差相差 ≥ 1 秒。
- 工头笔记：
  - `THE MAINSPRING IS AT THE HEART OF THIS FLOOR.`
  - `YOU NEVER GAIN A SECOND, APPRENTICE.`
  - `YOU ONLY CHOOSE WHEN TO MEET WHAT IS COMING.`
  - `REACH THE SPRING. THEN DO THE ONE THING I NEVER COULD.`
- 结局文字沿用现有的 `endingLines` 和 `endingSignOff`，再加上结算页（P9）。

### R8 THE LEDGER（可选挑战）

- 解锁条件：R1 到 R7 全部达到 `PAID IN FULL`。
- 3 个销钉，没有落点幽灵，par = 求解器最优 × 1.05。
- 只有一个房间这么做，几何直接重排 R6 的部件，不新增机制。
- 笔记：`THE FOREMAN'S OWN LEDGER. EVERY LINE OF IT IN RED.` / `CLOSE IT IN THE BLACK.`

---

## 6. 开发提示词

执行顺序：**P0 → P1 → (D1 决定后可选 P1b) → P2 → P3 → … → P8 → P9 → P10 → P11 → P12**。每条做完都要跑测试、提交并推送，再开始下一条。

### P0 通用上下文（每条提示词前都贴上）

```
项目：GameJam 仓库里的 escapement/，Haskell + Gloss 写的俯视角游戏。
分支：escapement-machine-rooms-7fhwo5，只在这个分支上提交和推送，不要碰 main。
工具链：GHC 9.6.7，cabal 3.12.1.0（cabal.project 固定了 with-compiler）。
测试：cabal test spec（只依赖 base）；cabal test probe --test-show-details=direct（路线搜索，慢）。
必须先读：docs/LEVELS.md（关卡设计）、docs/DESIGN.md（规则）、src/Game/Types.hs、src/Game/Level.hs、
src/Game/Logic.hs、src/Game/TimeDebt.hs、src/Game/Solution.hs、test/Main.hs、test/Probe.hs。
规则：
- 纯逻辑放在 src/，不能 import Gloss。房间数据放在 src/Game/Level.hs，文字全部放在 src/Game/Story.hs。
- 游戏内文字全部是英文大写，不能用问号（字体没有这个字形），每段笔记不超过 4 行。
- -Wall 下不能有警告。不要改 TimeDebt 的规则，除非提示词明确要求。
- 门和齿轮的状态只能是时钟的函数，不能存状态。
- 每个房间都要满足：至少 2 种销钉顺序可以通关；除教学房间外，不存在不用刹车的通关路线。
- docs/LEVELS.md 里的坐标是初稿，可以调整；调整后要把最终数字写回文档。
- 回复用中文，提交信息用英文。
```

### P1 最优借款求解器（先做这个，后面所有房间都靠它验证）

```
目标：回答“销钉顺序到底影响不影响借款”，并给每个房间提供 par 和“不用刹车能否通关”的判定。

在 test/ 下新建 Solver.hs（可以把公共部分抽成 test/SearchCommon.hs，供 Probe.hs 共用），
再在 escapement.cabal 里新增 test-suite solver。

要求：
1. 对给定的 Level 和销钉顺序，用 Dijkstra 搜索最小借款（gBorrowed）。代价相同时，再比用时。
   状态 = (玩家位置量化到 4 px 网格, tsClock mod levelCycle, tsDebt, tsMode, 已拿销钉集合)。
   转移必须调用真实的 stepGame（或 simulate 的同等逻辑），每步 9 种移动 × {按住, 不按}。
   不能自己另写一套规则。
2. 剪枝：Normal 状态下不按空格的移动代价为 0；用 0-1 BFS 或 Dijkstra 都可以。
   如果状态空间太大，按“销钉之间的分段”搜索：先算从每个销钉（以及起点）、
   每个进入相位出发，到下一个销钉的最小借款，再按顺序拼接。拼接时必须带上到达时的完整时钟和债务，不能假设到达时债务为 0。
3. 输出一张表：每个房间 × 每种顺序 → 最小借款（秒）、用时（秒）、冻结次数。
   另外报告“不按空格能否通关”（只用 Normal 转移搜索）。
4. 先在现有的 The Escape Wheel（mkRooms 第一个）上跑，把 6 种顺序的结果写进 docs/LEVELS.md 第 7 节，
   并给出结论：顺序是否影响借款，差多少。
5. 如果单个房间运行超过 10 分钟，先报告规模和瓶颈，不要硬跑。

验收：cabal test solver 能跑完 The Escape Wheel；结果和 roomBot 的脚本数据对照，最优借款不应高于脚本的 7.5 秒。
```

### P1b （只有 D1 决定加 L4 时才执行）销钉卡死齿轮

```
在 Level 里加一个字段 lvlPinSeizes :: [(Int, Int)]，表示“拔下第 i 个销钉后，第 h 个齿轮永久停住”。
停住的位置取拔销钉那一刻齿轮所在的位置。

实现约束：
- 齿轮位置仍然只由时钟和 gPins 决定，不新增可漂移的状态。
  做法：在 gPins 旁边记录每个销钉被拔时的时钟（例如 gPinClock :: [(Int, HazardTick)]），
  hazardPositions 查到被卡死的齿轮时，就用那个时钟算位置。
- 卡死的齿轮仍然会致命，碰到照样死。
- 渲染：卡死的齿轮变成灰色，不再画轨迹，并加一个“咔嗒”火花（在 Fx 里）。
- 加上 spec 测试：卡死后齿轮在任意时钟下位置不变；幽灵（settledPositions）也正确；没有对应销钉的齿轮行为不变。
- 用 P1 求解器在 R2 上做 A/B 对比（加与不加），把结果写进 docs/LEVELS.md 第 7 节。
```

### P2 房间 R1 THE PIN ROOM

```
按 docs/LEVELS.md 第 5 节 R1 的设计，在 src/Game/Level.hs 里实现 pinRoom，放在 mkRooms 的第 1 位
（The Escape Wheel 暂时留在后面）。文字放进 src/Game/Story.hs，名字叫 floorPinRoom。

步骤：
1. 画几何（wallsFromFloors），放 1 个齿轮、2 个销钉、出口、Zone 和 Prompt，lvlGhosts = False。
2. 用 P1 求解器验证：
   (a) 两种顺序都能通关；
   (b) 先 A 后 B 的借款明显少于先 B 后 A（预期冻结 1 次对 3 次）；
   (c) 不用刹车拿不到 B。
3. par = 最优借款 × 1.15 后取整。
4. 在 Solution.hs 里给它写一个 bot（可以复用 roomBot 的思路或 expandScript），两种顺序都要能通关；
   在 test/Main.hs 里加对应测试：两种顺序都 Won，最优顺序的借款 ≤ par。
5. 所有已有测试保持通过。把最终坐标和求解器数字写回文档。
```

### P3 房间 R2 THE CROSSING

```
按 docs/LEVELS.md 第 5 节 R2 实现 crossing，放在 mkRooms 第 2 位，文字放在 floorCrossing。

重点验证：
1. 臂宽 90 时，齿轮停在臂里确实堵死（写一个 spec 测试，检查玩家无法通过的那一段）。
2. 用求解器列出 6 种顺序的最小借款，必须满足第 2 节的顺序差标准。
   如果不满足，按下面的优先级调整：齿轮 oneWay → 臂长 → 北厅位置 → 销钉位置。每轮调整都要重跑求解器。
3. 不用刹车只能拿到 N，拿不到 W 和 E。
4. 3 种以上顺序都要有 bot 能通关，并加测试。
最后把调整过程（每轮改了什么、结果如何）简短记进文档第 7 节。
```

### P4 房间 R3 THE BELL DOORS

```
按 docs/LEVELS.md 第 5 节 R3 实现 bellDoors，放在 mkRooms 第 3 位，文字放在 floorBellDoors。

重点：
1. 先只放门和安全大厅，验证“在安全的地方等，门一定会开”（spec 测试遍历一个 levelCycle）。
2. 用求解器搜索三道门的 phase 组合（每个 phase 按步长 10 枚举），选出满足下面三条的组合：
   (a) 最好的顺序不是“近 → 远”；
   (b) 最好与最差的差距满足第 2 节的标准；
   (c) 至少有一个销钉必须靠“按住门”才能拿到（不用刹车搜不到）。
   把候选组合和得分写进文档。
3. 门压人的判定沿用 cfgDoorGrace，并加测试：门关上的那一刻站在门里会死，站在门边不会。
4. bot 至少覆盖 2 种顺序，并加测试。
```

### P5 房间 R4 THE COUNTING HOUSE

```
按 docs/LEVELS.md 第 5 节 R4 实现 countingHouse，放在 mkRooms 第 4 位，文字放在 floorCountingHouse。

重点：
1. 4 个格子用竖井围成一圈，外加一条斜向捷径。所有格子都必须是真正的安全区
   （spec 测试遍历完整周期，检查齿轮碰不到格子）。
2. 在 Probe 里加一个可达范围测试：任何时钟相位下，一次冻结都过不了两条相邻竖井。
3. 调整各竖井的 phase，使顺时针和逆时针的最小借款差 ≥ 0.5 秒，同时让捷径路线介于两者之间。
4. lvlGhosts = True，并检查：在每个格子里松手时，幽灵准确标出还清时的位置（复用已有的幽灵测试）。
5. bot 至少覆盖 2 种顺序，并加测试。
```

### P6 房间 R5 THE GALLERY RING

```
按 docs/LEVELS.md 第 5 节 R5 实现 galleryRing，放在 mkRooms 第 5 位，文字放在 floorGalleryRing。

重点：
1. 齿轮的 Route 是 4 个点的 U 形折线；确认 routePosition 能正确处理多段路线（加一个 spec 测试）。
2. 凹室必须安全（遍历周期验证），走廊宽 92，必须堵死（验证无法与齿轮擦身而过）。
3. 凹室故意不对称：用求解器证明“先东后西”比“先西后东”至少少借 0.5 秒。
   如果做不到，就移动凹 2 的位置，每次 20 px，重跑求解器。
4. 本关起不再显示玩家身边提示（lvlPrompts = []），只保留失败提示。
5. bot 至少覆盖 2 种顺序，并加测试。
```

### P7 房间 R6 THE ESCAPE WHEEL（调整）

```
按 docs/LEVELS.md 第 5 节 R6 调整现有的 escapeWheel，放在 mkRooms 第 6 位。

1. 先用求解器记录调整前的 6 种顺序结果（基线）。
2. 实施 L1：把南翼入口移到大厅东南角，让东长廊的齿轮停在合适位置时，能同时让出东、南两条路。
3. 实施 L3：南门的周期改成与东齿轮周期互质。
4. 重跑求解器，对比基线，必须满足第 2 节的标准。
5. 更新 roomBot（6 种顺序仍然全部通关），并更新 test/Main.hs 里相关的测试和 par。
6. 更新 floorEscapeWheel 的笔记和失败提示。
```

### P8 房间 R7 THE MAINSPRING 和可选的 R8

```
按 docs/LEVELS.md 第 5 节 R7 实现 mainspringRoom（4 个销钉），放在 mkRooms 第 7 位，文字放在 floorMainspringRoom。

1. 先用 ASCII 画出布局，标出每个区用了哪个机制，并在文档里确认之后再写代码。
2. 求解器要证明：至少 3 种顺序可以通关，最好与最差相差 ≥ 1 秒，并且不存在不用刹车的路线。
3. 最后一段必须边走边还：出口前的开放区里没有任何 Zone。
4. 4! = 24 种顺序，全部交给求解器算；bot 只需要覆盖最好和最差两种。
5. 如果时间允许，再做 R8 THE LEDGER：重排 R6 的部件，lvlGhosts = False，par = 最优 × 1.05，
   只有前 7 个房间全部 PAID IN FULL 才进入。
```

### P9 切换主流程、三档评价和结算页

```
1. 主流程改成：标题 → R1..R7（→ R8，如果解锁）→ 结局 → 结算页。
   原来的 5 层用 --classic 进入；--level=N 改为跳到第 N 个房间。
2. 评价分三档（docs/LEVELS.md 第 3 节 D2）：PAID IN FULL / IN GOOD STANDING / IN ARREARS。
   判定写成纯函数 rating :: Level -> Int -> Rating，放在 src/Game/，并加 spec 测试。
3. 每个房间过关时的横幅显示：评价、借款秒数、par 秒数。
4. Campaign 里记下每个房间最好的一次（借款、用时、死亡次数、评价）。
5. 结局之后加一个结算页：逐行列出房间名、评价、借款，最后一行是总借款和总评价。
   总评价按“最低的那一档”计。文字用英文大写，不用问号。
6. 更新 test/Main.hs 里跑完整流程的测试（campaignInputs），让它走完新的主流程。
```

### P10 剧情文字统一

```
把 docs/LEVELS.md 第 5 节的全部笔记和失败提示写进 src/Game/Story.hs。
通读一遍，检查：
- 工头的故事线连贯：R1 拿到刹车 → R2 停住齿轮的教训 → R3 门 → R4 贷款 → R5 三十年的自白 → R6 一个时钟 → R7 主发条；
- 每段 ≤ 4 行，每行不超过约 60 个字符（检查渲染宽度，不能超出屏幕）；
- 没有问号；
- 失败提示只给方向，不直接给出解法。
加一个 spec 测试：所有文本里都不含 '?'，每行长度不超过上限。
```

### P11 门开关反馈

```
在 src/Game/Fx.hs 里为门加反馈（纯逻辑，渲染放在 app/Render.hs）：
- 门由关变开、由开变关的那一帧，各产生一次短促的火花和一个环；
- 冻结中门保持原状时，门框上显示“被按住”的细线；
- 还债中门以 2 倍速开关时，粒子数要限制在 cfgMaxParticles 以内；
- cfgReducedEffects = True 时关掉所有装饰。
加测试：开关事件只在状态变化的那一帧触发，冻结期间不触发。
```

### P12 试玩记录

```
给 --log 模式加上房间级的记录：每次尝试的销钉顺序、借款、用时、死亡位置和死因。
退出时在工作目录写一个 CSV（文件名带时间戳）。再写一页简短的试玩说明，放在 docs/PLAYTEST.md：
找 3 到 5 个人，全程不说话；之后问 3 个开放问题（你为什么先拿那个销钉 / 什么时候觉得不公平 / 哪个房间最想再玩一次）。
```

---

## 7. 验证记录

（由 P1 到 P8 填写：每个房间 × 每种顺序的最小借款、用时、冻结次数、par，以及调整过程。）

| 房间 | 顺序 | 最小借款 (s) | 用时 (s) | 冻结次数 | 备注 |
|---|---|---|---|---|---|
| The Escape Wheel（调整前） | 6 种 | 待测（脚本：7.5 到 7.8） | 脚本：19 到 23 | | 基线 |
