# ETAF Performance Monitoring (性能监控)

## 概述 (Overview)

ETAF Performance Monitoring (etaf-perf) 是一个用于优化 ETAF 首屏加载时间的性能监控工具。它可以跟踪渲染管道每个阶段的执行时间，帮助开发者识别性能瓶颈并进行优化。

ETAF Performance Monitoring (etaf-perf) is a performance monitoring tool designed to optimize ETAF's first-screen loading time. It tracks the execution time of each stage in the rendering pipeline, helping developers identify performance bottlenecks and optimize accordingly.

## 功能特性 (Features)

### 监控的渲染阶段 (Monitored Rendering Stages)

etaf-perf 监控以下渲染管道阶段：

1. **check-dynamic-content** - 检查模板是否包含动态内容
2. **etml-to-dom** - ETML 转换为 DOM（静态模板）
3. **etml-compile-and-render** - ETML 编译和渲染（动态模板）
4. **build-stylesheet** - 构建样式表
5. **build-cssom** - 构建 CSS 对象模型
6. **add-stylesheet** - 添加额外样式表
7. **build-render-tree** - 构建渲染树
8. **build-layout-tree** - 构建布局树
9. **layout-to-string** - 布局转换为字符串

### 核心功能 (Core Features)

- ✅ 自动跟踪每个渲染阶段的执行时间
- ✅ 历史记录保存（可配置最大保存数量）
- ✅ 计算平均性能指标
- ✅ 生成详细的性能报告
- ✅ 性能瓶颈分析和优化建议
- ✅ 静态模板 vs 动态模板性能对比
- ✅ 零性能开销（禁用时）

## 安装 (Installation)

etaf-perf 模块已包含在 ETAF 中，无需额外安装：

```elisp
(require 'etaf-perf)
```

## 使用方法 (Usage)

### 基础用法 (Basic Usage)

```elisp
;; 1. 启用性能监控
(etaf-perf-enable)

;; 2. 安装监控钩子
(etaf-perf-install-hooks)

;; 3. 正常渲染内容（自动收集性能数据）
(etaf-paint-to-buffer "*demo*"
  '(div :class "container"
     (h1 "Hello ETAF!")
     (p "Performance monitoring is active.")))

;; 4. 查看性能报告
(etaf-perf-show-report)

;; 5. 分析性能瓶颈
(etaf-perf-analyze)

;; 6. 清除数据
(etaf-perf-clear)

;; 7. 禁用监控
(etaf-perf-disable)
```

### 查看最近的性能数据 (View Recent Performance Data)

```elisp
;; 获取最后一次测量
(etaf-perf-get-last)

;; 获取最近 N 次测量的平均值
(etaf-perf-get-average 10)

;; 显示包含平均值的报告
(etaf-perf-show-report 10)
```

### 手动测量代码段 (Manual Code Measurement)

```elisp
(etaf-perf-enable)
(etaf-perf-start)

;; 测量特定操作
(etaf-perf-measure my-operation
  (some-expensive-function))

(etaf-perf-measure another-operation
  (another-function))

(etaf-perf-finish)  ; 返回总执行时间
```

## 示例 (Examples)

### 示例 1: 简单模板性能测试 (Simple Template)

```elisp
(require 'etaf-perf)

(etaf-perf-enable)
(etaf-perf-install-hooks)
(etaf-perf-clear)

;; 渲染简单模板
(etaf-paint-to-buffer "*perf-test*"
  '(div :class "container"
     (h1 "Performance Test")
     (p "Testing performance monitoring.")
     (ul
      (li "Item 1")
      (li "Item 2")
      (li "Item 3"))))

;; 查看报告
(etaf-perf-show-report)
```

### 示例 2: 复杂 Tailwind 模板 (Complex Tailwind Template)

```elisp
(etaf-perf-enable)
(etaf-perf-install-hooks)
(etaf-perf-clear)

;; 渲染复杂模板
(etaf-paint-to-buffer "*perf-tailwind*"
  '(div :class "flex flex-col items-center w-800px bg-gray-100 p-4"
     (div :class "bg-white rounded-lg shadow-md p-4 mb-4"
       (h1 :class "text-2xl font-bold text-gray-900 mb-2"
           "Tailwind Performance")
       (p :class "text-gray-600" 
          "Testing with Tailwind classes."))
     (div :class "grid grid-cols-3 gap-4 w-full"
       (div :class "bg-blue-500 text-white p-3 rounded"
         (h3 :class "font-bold mb-1" "Fast")
         (p "Stage 1"))
       (div :class "bg-green-500 text-white p-3 rounded"
         (h3 :class "font-bold mb-1" "Faster")
         (p "Stage 2"))
       (div :class "bg-purple-500 text-white p-3 rounded"
         (h3 :class "font-bold mb-1" "Fastest")
         (p "Stage 3")))))

(etaf-perf-show-report)
```

### 示例 3: 动态模板性能测试 (Dynamic Template)

```elisp
(etaf-perf-enable)
(etaf-perf-install-hooks)
(etaf-perf-clear)

(let ((data '(:title "Dynamic Content"
              :items ("Apple" "Banana" "Cherry" "Date")
              :count 4)))
  
  (etaf-paint-to-buffer "*perf-dynamic*"
    '(div
       (h1 "{{ title }}")
       (p "Total items: {{ count }}")
       (ul
        (li :e-for "item in items" "• {{ item }}")))
    data))

(etaf-perf-show-report)
```

### 示例 4: 批量测试和平均性能 (Batch Testing)

```elisp
(etaf-perf-enable)
(etaf-perf-install-hooks)
(etaf-perf-clear)

;; 运行 20 次渲染
(dotimes (i 20)
  (etaf-paint-to-buffer "*perf-batch*"
    `(div :class "p-4"
       (h1 ,(format "Render #%d" (1+ i)))
       (div :class "grid grid-cols-4 gap-2"
         ,@(cl-loop for j from 1 to 16
                    collect `(div :class "bg-blue-100 p-2"
                               ,(format "Item %d" j)))))))

;; 查看平均性能
(etaf-perf-show-report 20)
```

### 示例 5: 性能对比 - 静态 vs 动态 (Static vs Dynamic Comparison)

```elisp
(etaf-perf-enable)
(etaf-perf-install-hooks)

;; 测试静态模板
(etaf-perf-clear)
(dotimes (i 10)
  (etaf-paint-to-buffer "*static*"
    '(div (h1 "Static") (p "Content"))))
(let ((static-time (plist-get (etaf-perf-get-average) :total)))
  
  ;; 测试动态模板
  (etaf-perf-clear)
  (dotimes (i 10)
    (etaf-paint-to-buffer "*dynamic*"
      '(div (h1 "{{ title }}") (p "{{ text }}"))
      '(:title "Dynamic" :text "Content")))
  (let ((dynamic-time (plist-get (etaf-perf-get-average) :total)))
    
    (message "Static: %.2f ms, Dynamic: %.2f ms, Overhead: %.1f%%"
             static-time
             dynamic-time
             (* 100.0 (/ (- dynamic-time static-time) static-time)))))
```

## 性能报告格式 (Report Format)

性能报告包含以下信息：

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
  Stages:
    check-dynamic-content          :   0.04 ms (  0.3%)
    etml-to-dom                    :   2.02 ms ( 13.6%)
    build-stylesheet               :   0.02 ms (  0.1%)
    build-cssom                    :   3.28 ms ( 22.1%)
    add-stylesheet                 :   0.17 ms (  1.1%)
    build-render-tree              :   4.15 ms ( 27.9%)
    build-layout-tree              :   3.76 ms ( 25.3%)
    layout-to-string               :   1.41 ms (  9.5%)

Total Measurements: 10
```

## 性能分析 (Performance Analysis)

### 识别瓶颈 (Identify Bottlenecks)

使用 `etaf-perf-analyze` 自动识别占用超过 30% 时间的阶段：

```elisp
(etaf-perf-analyze)
```

输出示例：
```
Performance Bottlenecks:
- build-render-tree takes 32.5% of total time (4.85 ms)
- build-layout-tree takes 30.2% of total time (4.51 ms)
```

### 优化建议 (Optimization Tips)

根据监控数据，可以采取以下优化措施：

1. **CSSOM 构建较慢**
   - 减少 CSS 规则数量
   - 使用 CSS 选择器索引（已自动优化）
   - 使用样式缓存

2. **渲染树构建较慢**
   - 简化 DOM 结构
   - 减少嵌套层级
   - 使用虚拟 DOM diff（动态模板）

3. **布局计算较慢**
   - 减少 Flexbox 嵌套
   - 使用固定尺寸而非自动计算
   - 避免复杂的布局结构

4. **动态模板编译较慢**
   - 尽可能使用静态模板
   - 缓存编译后的渲染函数
   - 减少模板指令使用

## API 参考 (API Reference)

### 控制函数 (Control Functions)

- `(etaf-perf-enable)` - 启用性能监控
- `(etaf-perf-disable)` - 禁用性能监控
- `(etaf-perf-clear)` - 清除所有性能数据
- `(etaf-perf-install-hooks)` - 安装监控钩子
- `(etaf-perf-uninstall-hooks)` - 卸载监控钩子

### 测量函数 (Measurement Functions)

- `(etaf-perf-start)` - 开始新的测量会话
- `(etaf-perf-record-stage STAGE START-TIME)` - 记录阶段时间
- `(etaf-perf-finish)` - 完成测量并保存到历史
- `(etaf-perf-measure STAGE &rest BODY)` - 测量代码块执行时间

### 查询函数 (Query Functions)

- `(etaf-perf-get-last)` - 获取最后一次测量
- `(etaf-perf-get-average &optional N)` - 获取平均性能指标
- `(etaf-perf-report &optional N)` - 生成性能报告字符串
- `(etaf-perf-show-report &optional N)` - 在缓冲区显示报告
- `(etaf-perf-analyze)` - 分析性能瓶颈

### 配置变量 (Configuration Variables)

- `etaf-perf-enabled` - 性能监控是否启用
- `etaf-perf-max-history` - 最大历史记录数（默认 100）

## 性能开销 (Performance Overhead)

- **禁用时**: 零开销（宏在编译时展开为 `progn`）
- **启用时**: 每个阶段约 0.01-0.05ms 的测量开销
- **建议**: 仅在开发和调试时启用，生产环境禁用

## 最佳实践 (Best Practices)

1. **开发阶段**
   ```elisp
   ;; 在 init.el 中
   (when (getenv "ETAF_PERF")
     (etaf-perf-enable)
     (etaf-perf-install-hooks))
   ```

2. **性能测试**
   ```elisp
   ;; 运行多次以获得稳定的平均值
   (dotimes (i 20)
     (etaf-paint-to-buffer "*test*" template))
   (etaf-perf-show-report 20)
   ```

3. **持续监控**
   ```elisp
   ;; 定期检查性能趋势
   (add-hook 'after-init-hook
     (lambda ()
       (when etaf-perf-enabled
         (run-with-timer 300 300 'etaf-perf-analyze))))
   ```

## 故障排除 (Troubleshooting)

### 问题: 没有性能数据

**解决方案:**
```elisp
;; 确保监控已启用
(etaf-perf-enable)

;; 确保钩子已安装
(etaf-perf-install-hooks)

;; 检查状态
(message "Enabled: %s, History: %d"
         etaf-perf-enabled
         (length etaf-perf-history))
```

### 问题: 性能数据不准确

**解决方案:**
- 运行多次测量并查看平均值
- 确保没有其他进程占用 CPU
- 在测量前清除缓存 `(etaf-perf-clear)`

## 相关资源 (Related Resources)

- [ETAF Architecture](ARCHITECTURE.md)
- [ETAF User Manual](USER-MANUAL.md)
- [Performance Examples](../examples/etaf-perf-example.el)

## 许可证 (License)

GNU General Public License v3.0 or later.
