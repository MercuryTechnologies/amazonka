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
-- Module      : Amazonka.APIGateway.Types.QuotaSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.QuotaSettings where

import Amazonka.APIGateway.Types.QuotaPeriodType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Quotas configured for a usage plan.
--
-- /See:/ 'newQuotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
  { -- | The day that a time period starts. For example, with a time period of
    -- @WEEK@, an offset of @0@ starts on Sunday, and an offset of @1@ starts
    -- on Monday.
    offset :: Prelude.Maybe Prelude.Int,
    -- | The time period in which the limit applies. Valid values are \"DAY\",
    -- \"WEEK\" or \"MONTH\".
    period :: Prelude.Maybe QuotaPeriodType,
    -- | The maximum number of requests that can be made in a given time period.
    limit :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuotaSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offset', 'quotaSettings_offset' - The day that a time period starts. For example, with a time period of
-- @WEEK@, an offset of @0@ starts on Sunday, and an offset of @1@ starts
-- on Monday.
--
-- 'period', 'quotaSettings_period' - The time period in which the limit applies. Valid values are \"DAY\",
-- \"WEEK\" or \"MONTH\".
--
-- 'limit', 'quotaSettings_limit' - The maximum number of requests that can be made in a given time period.
newQuotaSettings ::
  QuotaSettings
newQuotaSettings =
  QuotaSettings'
    { offset = Prelude.Nothing,
      period = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The day that a time period starts. For example, with a time period of
-- @WEEK@, an offset of @0@ starts on Sunday, and an offset of @1@ starts
-- on Monday.
quotaSettings_offset :: Lens.Lens' QuotaSettings (Prelude.Maybe Prelude.Int)
quotaSettings_offset = Lens.lens (\QuotaSettings' {offset} -> offset) (\s@QuotaSettings' {} a -> s {offset = a} :: QuotaSettings)

-- | The time period in which the limit applies. Valid values are \"DAY\",
-- \"WEEK\" or \"MONTH\".
quotaSettings_period :: Lens.Lens' QuotaSettings (Prelude.Maybe QuotaPeriodType)
quotaSettings_period = Lens.lens (\QuotaSettings' {period} -> period) (\s@QuotaSettings' {} a -> s {period = a} :: QuotaSettings)

-- | The maximum number of requests that can be made in a given time period.
quotaSettings_limit :: Lens.Lens' QuotaSettings (Prelude.Maybe Prelude.Int)
quotaSettings_limit = Lens.lens (\QuotaSettings' {limit} -> limit) (\s@QuotaSettings' {} a -> s {limit = a} :: QuotaSettings)

instance Core.FromJSON QuotaSettings where
  parseJSON =
    Core.withObject
      "QuotaSettings"
      ( \x ->
          QuotaSettings'
            Prelude.<$> (x Core..:? "offset")
            Prelude.<*> (x Core..:? "period")
            Prelude.<*> (x Core..:? "limit")
      )

instance Prelude.Hashable QuotaSettings where
  hashWithSalt salt' QuotaSettings' {..} =
    salt' `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` offset

instance Prelude.NFData QuotaSettings where
  rnf QuotaSettings' {..} =
    Prelude.rnf offset `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf period

instance Core.ToJSON QuotaSettings where
  toJSON QuotaSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("offset" Core..=) Prelude.<$> offset,
            ("period" Core..=) Prelude.<$> period,
            ("limit" Core..=) Prelude.<$> limit
          ]
      )
