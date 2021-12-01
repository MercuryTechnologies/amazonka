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
-- Module      : Amazonka.Redshift.Types.DataTransferProgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DataTransferProgress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes the status of a cluster while it is in the process of resizing
-- with an incremental resize.
--
-- /See:/ 'newDataTransferProgress' smart constructor.
data DataTransferProgress = DataTransferProgress'
  { -- | Describes the data transfer rate in MB\'s per second.
    currentRateInMegaBytesPerSecond :: Prelude.Maybe Prelude.Double,
    -- | Describes the status of the cluster. While the transfer is in progress
    -- the status is @transferringdata@.
    status :: Prelude.Maybe Prelude.Text,
    -- | Describes the estimated number of seconds remaining to complete the
    -- transfer.
    estimatedTimeToCompletionInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | Describes the total amount of data that has been transfered in MB\'s.
    dataTransferredInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | Describes the total amount of data to be transfered in megabytes.
    totalDataInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | Describes the number of seconds that have elapsed during the data
    -- transfer.
    elapsedTimeInSeconds :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataTransferProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentRateInMegaBytesPerSecond', 'dataTransferProgress_currentRateInMegaBytesPerSecond' - Describes the data transfer rate in MB\'s per second.
--
-- 'status', 'dataTransferProgress_status' - Describes the status of the cluster. While the transfer is in progress
-- the status is @transferringdata@.
--
-- 'estimatedTimeToCompletionInSeconds', 'dataTransferProgress_estimatedTimeToCompletionInSeconds' - Describes the estimated number of seconds remaining to complete the
-- transfer.
--
-- 'dataTransferredInMegaBytes', 'dataTransferProgress_dataTransferredInMegaBytes' - Describes the total amount of data that has been transfered in MB\'s.
--
-- 'totalDataInMegaBytes', 'dataTransferProgress_totalDataInMegaBytes' - Describes the total amount of data to be transfered in megabytes.
--
-- 'elapsedTimeInSeconds', 'dataTransferProgress_elapsedTimeInSeconds' - Describes the number of seconds that have elapsed during the data
-- transfer.
newDataTransferProgress ::
  DataTransferProgress
newDataTransferProgress =
  DataTransferProgress'
    { currentRateInMegaBytesPerSecond =
        Prelude.Nothing,
      status = Prelude.Nothing,
      estimatedTimeToCompletionInSeconds = Prelude.Nothing,
      dataTransferredInMegaBytes = Prelude.Nothing,
      totalDataInMegaBytes = Prelude.Nothing,
      elapsedTimeInSeconds = Prelude.Nothing
    }

-- | Describes the data transfer rate in MB\'s per second.
dataTransferProgress_currentRateInMegaBytesPerSecond :: Lens.Lens' DataTransferProgress (Prelude.Maybe Prelude.Double)
dataTransferProgress_currentRateInMegaBytesPerSecond = Lens.lens (\DataTransferProgress' {currentRateInMegaBytesPerSecond} -> currentRateInMegaBytesPerSecond) (\s@DataTransferProgress' {} a -> s {currentRateInMegaBytesPerSecond = a} :: DataTransferProgress)

-- | Describes the status of the cluster. While the transfer is in progress
-- the status is @transferringdata@.
dataTransferProgress_status :: Lens.Lens' DataTransferProgress (Prelude.Maybe Prelude.Text)
dataTransferProgress_status = Lens.lens (\DataTransferProgress' {status} -> status) (\s@DataTransferProgress' {} a -> s {status = a} :: DataTransferProgress)

-- | Describes the estimated number of seconds remaining to complete the
-- transfer.
dataTransferProgress_estimatedTimeToCompletionInSeconds :: Lens.Lens' DataTransferProgress (Prelude.Maybe Prelude.Integer)
dataTransferProgress_estimatedTimeToCompletionInSeconds = Lens.lens (\DataTransferProgress' {estimatedTimeToCompletionInSeconds} -> estimatedTimeToCompletionInSeconds) (\s@DataTransferProgress' {} a -> s {estimatedTimeToCompletionInSeconds = a} :: DataTransferProgress)

-- | Describes the total amount of data that has been transfered in MB\'s.
dataTransferProgress_dataTransferredInMegaBytes :: Lens.Lens' DataTransferProgress (Prelude.Maybe Prelude.Integer)
dataTransferProgress_dataTransferredInMegaBytes = Lens.lens (\DataTransferProgress' {dataTransferredInMegaBytes} -> dataTransferredInMegaBytes) (\s@DataTransferProgress' {} a -> s {dataTransferredInMegaBytes = a} :: DataTransferProgress)

-- | Describes the total amount of data to be transfered in megabytes.
dataTransferProgress_totalDataInMegaBytes :: Lens.Lens' DataTransferProgress (Prelude.Maybe Prelude.Integer)
dataTransferProgress_totalDataInMegaBytes = Lens.lens (\DataTransferProgress' {totalDataInMegaBytes} -> totalDataInMegaBytes) (\s@DataTransferProgress' {} a -> s {totalDataInMegaBytes = a} :: DataTransferProgress)

-- | Describes the number of seconds that have elapsed during the data
-- transfer.
dataTransferProgress_elapsedTimeInSeconds :: Lens.Lens' DataTransferProgress (Prelude.Maybe Prelude.Integer)
dataTransferProgress_elapsedTimeInSeconds = Lens.lens (\DataTransferProgress' {elapsedTimeInSeconds} -> elapsedTimeInSeconds) (\s@DataTransferProgress' {} a -> s {elapsedTimeInSeconds = a} :: DataTransferProgress)

instance Core.FromXML DataTransferProgress where
  parseXML x =
    DataTransferProgress'
      Prelude.<$> (x Core..@? "CurrentRateInMegaBytesPerSecond")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "EstimatedTimeToCompletionInSeconds")
      Prelude.<*> (x Core..@? "DataTransferredInMegaBytes")
      Prelude.<*> (x Core..@? "TotalDataInMegaBytes")
      Prelude.<*> (x Core..@? "ElapsedTimeInSeconds")

instance Prelude.Hashable DataTransferProgress where
  hashWithSalt salt' DataTransferProgress' {..} =
    salt' `Prelude.hashWithSalt` elapsedTimeInSeconds
      `Prelude.hashWithSalt` totalDataInMegaBytes
      `Prelude.hashWithSalt` dataTransferredInMegaBytes
      `Prelude.hashWithSalt` estimatedTimeToCompletionInSeconds
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` currentRateInMegaBytesPerSecond

instance Prelude.NFData DataTransferProgress where
  rnf DataTransferProgress' {..} =
    Prelude.rnf currentRateInMegaBytesPerSecond
      `Prelude.seq` Prelude.rnf elapsedTimeInSeconds
      `Prelude.seq` Prelude.rnf totalDataInMegaBytes
      `Prelude.seq` Prelude.rnf dataTransferredInMegaBytes
      `Prelude.seq` Prelude.rnf estimatedTimeToCompletionInSeconds
      `Prelude.seq` Prelude.rnf status
