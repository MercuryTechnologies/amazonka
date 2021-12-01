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
-- Module      : Amazonka.EC2.Types.DestinationOptionsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DestinationOptionsResponse where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DestinationFileFormat
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination options for a flow log.
--
-- /See:/ 'newDestinationOptionsResponse' smart constructor.
data DestinationOptionsResponse = DestinationOptionsResponse'
  { -- | Indicates whether to partition the flow log per hour.
    perHourPartition :: Prelude.Maybe Prelude.Bool,
    -- | The format for the flow log.
    fileFormat :: Prelude.Maybe DestinationFileFormat,
    -- | Indicates whether to use Hive-compatible prefixes for flow logs stored
    -- in Amazon S3.
    hiveCompatiblePartitions :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'perHourPartition', 'destinationOptionsResponse_perHourPartition' - Indicates whether to partition the flow log per hour.
--
-- 'fileFormat', 'destinationOptionsResponse_fileFormat' - The format for the flow log.
--
-- 'hiveCompatiblePartitions', 'destinationOptionsResponse_hiveCompatiblePartitions' - Indicates whether to use Hive-compatible prefixes for flow logs stored
-- in Amazon S3.
newDestinationOptionsResponse ::
  DestinationOptionsResponse
newDestinationOptionsResponse =
  DestinationOptionsResponse'
    { perHourPartition =
        Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      hiveCompatiblePartitions = Prelude.Nothing
    }

-- | Indicates whether to partition the flow log per hour.
destinationOptionsResponse_perHourPartition :: Lens.Lens' DestinationOptionsResponse (Prelude.Maybe Prelude.Bool)
destinationOptionsResponse_perHourPartition = Lens.lens (\DestinationOptionsResponse' {perHourPartition} -> perHourPartition) (\s@DestinationOptionsResponse' {} a -> s {perHourPartition = a} :: DestinationOptionsResponse)

-- | The format for the flow log.
destinationOptionsResponse_fileFormat :: Lens.Lens' DestinationOptionsResponse (Prelude.Maybe DestinationFileFormat)
destinationOptionsResponse_fileFormat = Lens.lens (\DestinationOptionsResponse' {fileFormat} -> fileFormat) (\s@DestinationOptionsResponse' {} a -> s {fileFormat = a} :: DestinationOptionsResponse)

-- | Indicates whether to use Hive-compatible prefixes for flow logs stored
-- in Amazon S3.
destinationOptionsResponse_hiveCompatiblePartitions :: Lens.Lens' DestinationOptionsResponse (Prelude.Maybe Prelude.Bool)
destinationOptionsResponse_hiveCompatiblePartitions = Lens.lens (\DestinationOptionsResponse' {hiveCompatiblePartitions} -> hiveCompatiblePartitions) (\s@DestinationOptionsResponse' {} a -> s {hiveCompatiblePartitions = a} :: DestinationOptionsResponse)

instance Core.FromXML DestinationOptionsResponse where
  parseXML x =
    DestinationOptionsResponse'
      Prelude.<$> (x Core..@? "perHourPartition")
      Prelude.<*> (x Core..@? "fileFormat")
      Prelude.<*> (x Core..@? "hiveCompatiblePartitions")

instance Prelude.Hashable DestinationOptionsResponse where
  hashWithSalt salt' DestinationOptionsResponse' {..} =
    salt'
      `Prelude.hashWithSalt` hiveCompatiblePartitions
      `Prelude.hashWithSalt` fileFormat
      `Prelude.hashWithSalt` perHourPartition

instance Prelude.NFData DestinationOptionsResponse where
  rnf DestinationOptionsResponse' {..} =
    Prelude.rnf perHourPartition
      `Prelude.seq` Prelude.rnf hiveCompatiblePartitions
      `Prelude.seq` Prelude.rnf fileFormat
