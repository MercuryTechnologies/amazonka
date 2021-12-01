{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.Types.BatchUpdateDevicePositionError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchUpdateDevicePositionError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.BatchItemError
import qualified Amazonka.Prelude as Prelude

-- | Contains error details for each device that failed to update its
-- position.
--
-- /See:/ 'newBatchUpdateDevicePositionError' smart constructor.
data BatchUpdateDevicePositionError = BatchUpdateDevicePositionError'
  { -- | The device associated with the failed location update.
    deviceId :: Prelude.Text,
    -- | Contains details related to the error code such as the error code and
    -- error message.
    error :: BatchItemError,
    -- | The timestamp at which the device position was determined. Uses
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    sampleTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateDevicePositionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'batchUpdateDevicePositionError_deviceId' - The device associated with the failed location update.
--
-- 'error', 'batchUpdateDevicePositionError_error' - Contains details related to the error code such as the error code and
-- error message.
--
-- 'sampleTime', 'batchUpdateDevicePositionError_sampleTime' - The timestamp at which the device position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newBatchUpdateDevicePositionError ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'error'
  BatchItemError ->
  -- | 'sampleTime'
  Prelude.UTCTime ->
  BatchUpdateDevicePositionError
newBatchUpdateDevicePositionError
  pDeviceId_
  pError_
  pSampleTime_ =
    BatchUpdateDevicePositionError'
      { deviceId =
          pDeviceId_,
        error = pError_,
        sampleTime = Core._Time Lens.# pSampleTime_
      }

-- | The device associated with the failed location update.
batchUpdateDevicePositionError_deviceId :: Lens.Lens' BatchUpdateDevicePositionError Prelude.Text
batchUpdateDevicePositionError_deviceId = Lens.lens (\BatchUpdateDevicePositionError' {deviceId} -> deviceId) (\s@BatchUpdateDevicePositionError' {} a -> s {deviceId = a} :: BatchUpdateDevicePositionError)

-- | Contains details related to the error code such as the error code and
-- error message.
batchUpdateDevicePositionError_error :: Lens.Lens' BatchUpdateDevicePositionError BatchItemError
batchUpdateDevicePositionError_error = Lens.lens (\BatchUpdateDevicePositionError' {error} -> error) (\s@BatchUpdateDevicePositionError' {} a -> s {error = a} :: BatchUpdateDevicePositionError)

-- | The timestamp at which the device position was determined. Uses
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
batchUpdateDevicePositionError_sampleTime :: Lens.Lens' BatchUpdateDevicePositionError Prelude.UTCTime
batchUpdateDevicePositionError_sampleTime = Lens.lens (\BatchUpdateDevicePositionError' {sampleTime} -> sampleTime) (\s@BatchUpdateDevicePositionError' {} a -> s {sampleTime = a} :: BatchUpdateDevicePositionError) Prelude.. Core._Time

instance Core.FromJSON BatchUpdateDevicePositionError where
  parseJSON =
    Core.withObject
      "BatchUpdateDevicePositionError"
      ( \x ->
          BatchUpdateDevicePositionError'
            Prelude.<$> (x Core..: "DeviceId")
            Prelude.<*> (x Core..: "Error")
            Prelude.<*> (x Core..: "SampleTime")
      )

instance
  Prelude.Hashable
    BatchUpdateDevicePositionError
  where
  hashWithSalt
    salt'
    BatchUpdateDevicePositionError' {..} =
      salt' `Prelude.hashWithSalt` sampleTime
        `Prelude.hashWithSalt` error
        `Prelude.hashWithSalt` deviceId

instance
  Prelude.NFData
    BatchUpdateDevicePositionError
  where
  rnf BatchUpdateDevicePositionError' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf sampleTime
      `Prelude.seq` Prelude.rnf error
