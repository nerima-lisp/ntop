(in-package #:ntop.memory.domain)

(ntop.shared.domain:define-sample-type memory-sample
                                       (ram-used-bytes nil
                                                       :type
                                                       (or null
                                                           ntop.shared.domain:measurement-counter))
                                       (ram-total-bytes nil
                                                        :type
                                                        (or null
                                                            ntop.shared.domain:measurement-counter))
                                       (swap-used-bytes nil
                                                        :type
                                                        (or null
                                                            ntop.shared.domain:measurement-counter))
                                       (swap-total-bytes nil
                                                         :type
                                                         (or
                                                          null
                                                          ntop.shared.domain:measurement-counter))
                                       (cache-used-bytes nil
                                                         :type
                                                         (or
                                                          null
                                                          ntop.shared.domain:measurement-counter))
                                       (cache-total-bytes nil
                                                          :type
                                                          (or
                                                           null
                                                           ntop.shared.domain:measurement-counter))
                                       (arc-used-bytes nil :type null)
                                       (arc-total-bytes nil :type null))
