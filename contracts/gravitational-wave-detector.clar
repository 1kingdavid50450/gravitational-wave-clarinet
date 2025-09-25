;; Gravitational Wave Detector Contract
;; Ultra-sensitive LIGO-style detector for capturing cosmic ripples in spacetime

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-strain (err u101))
(define-constant err-invalid-frequency (err u102))
(define-constant err-detector-offline (err u103))
(define-constant err-calibration-required (err u104))
(define-constant err-detection-not-found (err u105))
(define-constant err-insufficient-sensitivity (err u106))

;; Minimum detectable strain amplitude (10^-21)
(define-constant min-strain-amplitude u21)
;; Maximum frequency range (7000 Hz)
(define-constant max-frequency u7000)
;; Minimum frequency range (10 Hz)
(define-constant min-frequency u10)
;; Calibration interval in blocks
(define-constant calibration-interval u144)

;; Data Variables
(define-data-var detector-status bool true)
(define-data-var current-sensitivity uint u21)
(define-data-var last-calibration uint u0)
(define-data-var detection-counter uint u0)
(define-data-var total-strain-measured uint u0)
(define-data-var laser-power uint u100)
(define-data-var interferometer-length uint u4000)

;; Maps
(define-map wave-detections
  uint
  {
    timestamp: uint,
    strain-amplitude: uint,
    frequency: uint,
    duration: uint,
    event-type: (string-ascii 50),
    confidence: uint,
    detector-id: uint,
    processed: bool
  }
)

(define-map detector-calibration
  uint
  {
    calibration-time: uint,
    sensitivity-level: uint,
    laser-stability: uint,
    noise-floor: uint,
    operator: principal
  }
)

(define-map event-classifications
  (string-ascii 50)
  {
    frequency-range-min: uint,
    frequency-range-max: uint,
    typical-strain: uint,
    classification-confidence: uint
  }
)

;; Initialize event classifications
(map-set event-classifications "black-hole-merger" 
  {
    frequency-range-min: u20,
    frequency-range-max: u250,
    typical-strain: u21,
    classification-confidence: u95
  }
)

(map-set event-classifications "neutron-star-collision"
  {
    frequency-range-min: u40,
    frequency-range-max: u2000,
    typical-strain: u22,
    classification-confidence: u90
  }
)

(map-set event-classifications "binary-inspiral"
  {
    frequency-range-min: u10,
    frequency-range-max: u400,
    typical-strain: u21,
    classification-confidence: u85
  }
)

(map-set event-classifications "pulsar-glitch"
  {
    frequency-range-min: u100,
    frequency-range-max: u1500,
    typical-strain: u23,
    classification-confidence: u80
  }
)

;; Public Functions

;; Calibrate the detector system
(define-public (calibrate-detector (sensitivity uint) (laser-power-level uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= sensitivity min-strain-amplitude) err-insufficient-sensitivity)
    (asserts! (<= laser-power-level u200) (err u107))
    
    (var-set current-sensitivity sensitivity)
    (var-set laser-power laser-power-level)
    (var-set last-calibration stacks-block-height)
    
    (map-set detector-calibration stacks-block-height
      {
        calibration-time: stacks-block-height,
        sensitivity-level: sensitivity,
        laser-stability: laser-power-level,
        noise-floor: (/ sensitivity u10),
        operator: tx-sender
      }
    )
    
    (ok "Detector calibrated successfully")
  )
)

;; Register a new gravitational wave detection
(define-public (register-detection 
    (strain uint) 
    (frequency uint) 
    (duration uint) 
    (event-type (string-ascii 50))
  )
  (let 
    (
      (detection-id (+ (var-get detection-counter) u1))
      (current-time stacks-block-height)
      (sensitivity (var-get current-sensitivity))
    )
    
    ;; Validate detector is online
    (asserts! (var-get detector-status) err-detector-offline)
    
    ;; Validate calibration is recent
    (asserts! 
      (<= (- stacks-block-height (var-get last-calibration)) calibration-interval)
      err-calibration-required
    )
    
    ;; Validate strain amplitude is detectable
    (asserts! (>= strain sensitivity) err-invalid-strain)
    
    ;; Validate frequency range
    (asserts! (and (>= frequency min-frequency) (<= frequency max-frequency)) err-invalid-frequency)
    
    ;; Calculate confidence based on strain strength
    (let ((confidence (calculate-confidence strain frequency)))
      
      ;; Store the detection
      (map-set wave-detections detection-id
        {
          timestamp: current-time,
          strain-amplitude: strain,
          frequency: frequency,
          duration: duration,
          event-type: event-type,
          confidence: confidence,
          detector-id: u1,
          processed: false
        }
      )
      
      ;; Update counters
      (var-set detection-counter detection-id)
      (var-set total-strain-measured (+ (var-get total-strain-measured) strain))
      
      (ok detection-id)
    )
  )
)

;; Get detection details
(define-read-only (get-detection (detection-id uint))
  (map-get? wave-detections detection-id)
)

;; Get detector status information
(define-read-only (get-detector-status)
  {
    online: (var-get detector-status),
    sensitivity: (var-get current-sensitivity),
    last-calibration: (var-get last-calibration),
    total-detections: (var-get detection-counter),
    laser-power: (var-get laser-power),
    interferometer-length: (var-get interferometer-length)
  }
)

;; Mark detection as processed
(define-public (mark-processed (detection-id uint))
  (let ((detection (unwrap! (map-get? wave-detections detection-id) err-detection-not-found)))
    (map-set wave-detections detection-id
      (merge detection { processed: true })
    )
    (ok true)
  )
)

;; Update detector status
(define-public (set-detector-status (online bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set detector-status online)
    (ok online)
  )
)

;; Get recent detections in a range
(define-read-only (get-recent-detections (start-id uint) (count uint))
  (let ((current-count (var-get detection-counter)))
    (if (<= start-id current-count)
      (some {
        start: start-id,
        end: (min-value (+ start-id count) current-count),
        total-available: current-count
      })
      none
    )
  )
)

;; Calculate event classification confidence
(define-read-only (classify-event (frequency uint) (strain uint) (duration uint))
  (let ((classification-data (map-get? event-classifications "black-hole-merger")))
    (match classification-data
      classification
      (if (and 
            (>= frequency (get frequency-range-min classification))
            (<= frequency (get frequency-range-max classification))
            (>= strain (get typical-strain classification))
          )
          (some {
            event-type: "black-hole-merger",
            confidence: (get classification-confidence classification)
          })
          (classify-neutron-star frequency strain duration)
      )
      (classify-neutron-star frequency strain duration)
    )
  )
)

;; Helper function for neutron star classification
(define-read-only (classify-neutron-star (frequency uint) (strain uint) (duration uint))
  (let ((classification-data (map-get? event-classifications "neutron-star-collision")))
    (match classification-data
      classification
      (if (and 
            (>= frequency (get frequency-range-min classification))
            (<= frequency (get frequency-range-max classification))
            (>= strain (get typical-strain classification))
          )
          (some {
            event-type: "neutron-star-collision",
            confidence: (get classification-confidence classification)
          })
          (some {
            event-type: "unknown",
            confidence: u50
          })
      )
      (some {
        event-type: "unknown",
        confidence: u50
      })
    )
  )
)

;; Private Functions

;; Helper function to calculate minimum of two values
(define-private (min-value (a uint) (b uint))
  (if (<= a b) a b)
)

;; Helper function to calculate maximum of two values
(define-private (max-value (a uint) (b uint))
  (if (>= a b) a b)
)

;; Calculate detection confidence based on strain and frequency
(define-private (calculate-confidence (strain uint) (frequency uint))
  (let 
    (
      (sensitivity (var-get current-sensitivity))
      (strain-ratio (/ (* strain u100) sensitivity))
      (frequency-factor (if (and (>= frequency u20) (<= frequency u2000)) u100 u80))
    )
    ;; Base confidence on strain strength and frequency range
    (min-value u100 (+ (/ strain-ratio u2) (/ frequency-factor u2)))
  )
)

;; Get calibration data
(define-read-only (get-calibration-data (block-height-ref uint))
  (map-get? detector-calibration block-height-ref)
)

;; Get event classification parameters
(define-read-only (get-event-parameters (event-type (string-ascii 50)))
  (map-get? event-classifications event-type)
)

;; Calculate noise floor based on current sensitivity
(define-read-only (get-noise-floor)
  (/ (var-get current-sensitivity) u5)
)

;; Check if detector needs recalibration
(define-read-only (needs-calibration)
  (> (- stacks-block-height (var-get last-calibration)) calibration-interval)
)

;; Get detection statistics
(define-read-only (get-detection-statistics)
  {
    total-detections: (var-get detection-counter),
    total-strain: (var-get total-strain-measured),
    average-strain: (if (> (var-get detection-counter) u0)
                      (/ (var-get total-strain-measured) (var-get detection-counter))
                      u0),
    detector-uptime: (- stacks-block-height (var-get last-calibration))
  }
)


;; title: gravitational-wave-detector
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

