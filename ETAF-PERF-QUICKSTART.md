# ETAF Performance Monitoring Quick Start

## 快速开始 (Quick Start)

### 1. 基础使用 (Basic Usage)

```elisp
;; 加载性能监控模块
(require 'etaf-perf)

;; 启用性能监控
(etaf-perf-enable)

;; 安装监控钩子
(etaf-perf-install-hooks)

;; 正常渲染内容
(etaf-paint-to-buffer "*demo*"
  '(div :class "container"
     (h1 "Performance Test")
     (p "Testing performance monitoring.")))

;; 查看性能报告
(etaf-perf-show-report)
```

### 2. 运行示例 (Run Examples)

```elisp
;; 加载示例文件
(load-file "examples/etaf-perf-example.el")

;; 运行示例 1: 基础监控
M-x etaf-perf-example-1-basic

;; 运行示例 2: 复杂模板
M-x etaf-perf-example-2-complex

;; 运行示例 3: 动态模板
M-x etaf-perf-example-3-dynamic

;; 运行示例 4: 批量测试
M-x etaf-perf-example-4-multiple

;; 运行示例 5: 性能分析
M-x etaf-perf-example-5-analyze

;; 运行示例 6: 静态 vs 动态对比
M-x etaf-perf-example-6-comparison

;; 查看所有示例
M-x etaf-perf-demo
```

### 3. 性能分析 (Performance Analysis)

```elisp
;; 启用监控
(etaf-perf-enable)
(etaf-perf-install-hooks)

;; 渲染多次以获得平均数据
(dotimes (i 10)
  (etaf-paint-to-buffer "*test*"
    '(div :class "flex flex-col p-4"
       (h1 :class "text-2xl font-bold" "Test")
       (div :class "grid grid-cols-3 gap-4"
         ,@(cl-loop for j from 1 to 20
                    collect `(div :class "bg-blue-100 p-2"
                               ,(format "Item %d" j)))))))

;; 查看平均性能报告
(etaf-perf-show-report 10)

;; 分析性能瓶颈
(etaf-perf-analyze)
```

### 4. 性能对比 (Performance Comparison)

```elisp
;; 测试静态模板
(etaf-perf-clear)
(dotimes (i 10)
  (etaf-paint-to-buffer "*static*"
    '(div (h1 "Static Template") (p "Content"))))
(setq static-avg (etaf-perf-get-average))

;; 测试动态模板
(etaf-perf-clear)
(dotimes (i 10)
  (etaf-paint-to-buffer "*dynamic*"
    '(div (h1 "{{ title }}") (p "{{ content }}"))
    '(:title "Dynamic Template" :content "Content")))
(setq dynamic-avg (etaf-perf-get-average))

;; 比较结果
(message "Static: %.2f ms, Dynamic: %.2f ms"
         (plist-get static-avg :total)
         (plist-get dynamic-avg :total))
```

## 常用命令 (Common Commands)

| 命令 | 说明 |
|------|------|
| `M-x etaf-perf-enable` | 启用性能监控 |
| `M-x etaf-perf-disable` | 禁用性能监控 |
| `M-x etaf-perf-clear` | 清除性能数据 |
| `M-x etaf-perf-show-report` | 显示性能报告 |
| `M-x etaf-perf-analyze` | 分析性能瓶颈 |
| `M-x etaf-perf-demo` | 查看示例列表 |

## 性能报告示例 (Sample Report)

```
=== ETAF Performance Report ===

Last Measurement:
  Total Time: 15.42 ms
  Stages:
    check-dynamic-content          :   0.05 ms (  0.3%)
    etml-to-dom                    :   2.15 ms ( 13.9%)
    build-stylesheet               :   0.02 ms (  0.1%)
    build-cssom                    :   3.45 ms ( 22.4%)
    add-stylesheet                 :   0.18 ms (  1.2%)
    build-render-tree              :   4.23 ms ( 27.4%)
    build-layout-tree              :   3.89 ms ( 25.2%)
    layout-to-string               :   1.45 ms (  9.4%)

Average of Last 10 Measurements:
  Total Time: 14.85 ms
  (Similar breakdown...)

Total Measurements: 10
```

## 优化建议 (Optimization Tips)

根据性能报告，针对性地优化：

1. **CSSOM 构建慢** → 减少 CSS 规则
2. **渲染树构建慢** → 简化 DOM 结构
3. **布局计算慢** → 减少 Flexbox 嵌套
4. **动态编译慢** → 使用静态模板

## 更多信息 (More Information)

详细文档请参阅: [docs/ETAF-PERF.md](../docs/ETAF-PERF.md)
