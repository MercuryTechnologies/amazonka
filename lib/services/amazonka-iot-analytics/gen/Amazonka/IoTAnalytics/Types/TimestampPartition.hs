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
-- Module      : Amazonka.IoTAnalytics.Types.TimestampPartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.TimestampPartition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A partition dimension defined by a timestamp attribute.
--
-- /See:/ 'newTimestampPartition' smart constructor.
data TimestampPartition = TimestampPartition'
  { -- | The timestamp format of a partition defined by a timestamp. The default
    -- format is seconds since epoch (January 1, 1970 at midnight UTC time).
    timestampFormat :: Prelude.Maybe Prelude.Text,
    -- | The attribute name of the partition defined by a timestamp.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestampPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestampFormat', 'timestampPartition_timestampFormat' - The timestamp format of a partition defined by a timestamp. The default
-- format is seconds since epoch (January 1, 1970 at midnight UTC time).
--
-- 'attributeName', 'timestampPartition_attributeName' - The attribute name of the partition defined by a timestamp.
newTimestampPartition ::
  -- | 'attributeName'
  Prelude.Text ->
  TimestampPartition
newTimestampPartition pAttributeName_ =
  TimestampPartition'
    { timestampFormat =
        Prelude.Nothing,
      attributeName = pAttributeName_
    }

-- | The timestamp format of a partition defined by a timestamp. The default
-- format is seconds since epoch (January 1, 1970 at midnight UTC time).
timestampPartition_timestampFormat :: Lens.Lens' TimestampPartition (Prelude.Maybe Prelude.Text)
timestampPartition_timestampFormat = Lens.lens (\TimestampPartition' {timestampFormat} -> timestampFormat) (\s@TimestampPartition' {} a -> s {timestampFormat = a} :: TimestampPartition)

-- | The attribute name of the partition defined by a timestamp.
timestampPartition_attributeName :: Lens.Lens' TimestampPartition Prelude.Text
timestampPartition_attributeName = Lens.lens (\TimestampPartition' {attributeName} -> attributeName) (\s@TimestampPartition' {} a -> s {attributeName = a} :: TimestampPartition)

instance Core.FromJSON TimestampPartition where
  parseJSON =
    Core.withObject
      "TimestampPartition"
      ( \x ->
          TimestampPartition'
            Prelude.<$> (x Core..:? "timestampFormat")
            Prelude.<*> (x Core..: "attributeName")
      )

instance Prelude.Hashable TimestampPartition where
  hashWithSalt salt' TimestampPartition' {..} =
    salt' `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` timestampFormat

instance Prelude.NFData TimestampPartition where
  rnf TimestampPartition' {..} =
    Prelude.rnf timestampFormat
      `Prelude.seq` Prelude.rnf attributeName

instance Core.ToJSON TimestampPartition where
  toJSON TimestampPartition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timestampFormat" Core..=)
              Prelude.<$> timestampFormat,
            Prelude.Just
              ("attributeName" Core..= attributeName)
          ]
      )
