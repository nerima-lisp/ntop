(define-ntop-package #:ntop.platform.darwin
  (:documentation "Darwin system calls and bindings.")
  (:export
   ;; WP0-7: FFI declarations, sysctl, mach, CoreFoundation and IOKit.
   #:define-darwin-ffi #:decode-little-endian-unsigned #:decode-little-endian-signed
   #:decode-c-string #:decode-mach-timebase #:mach-ticks-to-nanoseconds #:sysctl-read-by-name
   #:sysctl-read-mib #:sysctl-read-integer-by-name #:sysctl-read-string-by-name
   #:call-with-mach-host-port #:mach-timebase-info #:mach-vm-deallocate
   #:mach-read-load-average #:mach-read-host-vm-statistics64
   #:darwin-error-string #:cf-null-reference-p #:call-with-owned-cf-reference
   #:with-owned-cf-reference #:cf-value-to-lisp #:cf-dictionary-get-value
   #:call-with-iokit-services #:iokit-registry-properties
   ;; F1, infrastructure-ioreport.lisp: IOReport bindings.
   ;; F3, infrastructure-ioaccel.lisp: IOAccelerate bindings.
   #:darwin-read-gpu-device-data
   ;; F4, infrastructure-ifmib.lisp: interface MIB bindings.
   #:decode-ifmibdata #:darwin-read-network-interface-data
   ;; F5, infrastructure-fs.lisp: filesystem and block-storage bindings.
   #:decode-darwin-statfs-buffer #:decode-darwin-volume-space-used
   #:read-darwin-file-system-stats #:read-darwin-volume-space-used
   #:read-darwin-block-storage-stats
   ;; F6, infrastructure-iohid.lisp: IOHID bindings.
   #:load-darwin-iohid-api
   ;; F7, infrastructure-power.lisp: power bindings.
   ;; F8a, infrastructure-libproc.lisp: libproc bindings.
   #:read-darwin-process-rows))

(define-ntop-package #:ntop.platform.linux
  (:documentation "Linux procfs and sysfs access.")
  (:export
   ;; WP9: Linux platform bindings.
   #:*linux-root* #:with-linux-root #:read-linux-proc-stat #:read-linux-proc-loadavg
   #:read-linux-proc-meminfo #:read-linux-cgroup-data #:read-linux-process-stat
   #:read-linux-process-status #:read-linux-process-io #:read-linux-process-cmdline
   #:read-linux-process-tasks #:read-proc-mounts #:read-proc-diskstats #:read-linux-statvfs
   #:read-linux-network-statistics #:read-linux-cpu-frequency-khz
   #:enumerate-linux-temperature-sources #:read-linux-temperature-values
   #:read-linux-batteries #:read-linux-amd-gpus))
