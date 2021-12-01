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
-- Module      : Amazonka.APIGateway.Types.Usage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.Usage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the usage data of a usage plan.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>,
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage Manage Usage in a Usage Plan>
--
-- /See:/ 'newUsage' smart constructor.
data Usage = Usage'
  { -- | The plan Id associated with this usage data.
    usagePlanId :: Prelude.Maybe Prelude.Text,
    -- | The ending date of the usage data.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The usage data, as daily logs of used and remaining quotas, over the
    -- specified time interval indexed over the API keys in a usage plan. For
    -- example,
    -- @{..., \"values\" : { \"{api_key}\" : [ [0, 100], [10, 90], [100, 10]]}@,
    -- where @{api_key}@ stands for an API key value and the daily log entry is
    -- of the format @[used quota, remaining quota]@.
    items :: Prelude.Maybe (Prelude.HashMap Prelude.Text [[Prelude.Integer]]),
    -- | The starting date of the usage data.
    startDate :: Prelude.Maybe Prelude.Text,
    position :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Usage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'usage_usagePlanId' - The plan Id associated with this usage data.
--
-- 'endDate', 'usage_endDate' - The ending date of the usage data.
--
-- 'items', 'usage_items' - The usage data, as daily logs of used and remaining quotas, over the
-- specified time interval indexed over the API keys in a usage plan. For
-- example,
-- @{..., \"values\" : { \"{api_key}\" : [ [0, 100], [10, 90], [100, 10]]}@,
-- where @{api_key}@ stands for an API key value and the daily log entry is
-- of the format @[used quota, remaining quota]@.
--
-- 'startDate', 'usage_startDate' - The starting date of the usage data.
--
-- 'position', 'usage_position' - Undocumented member.
newUsage ::
  Usage
newUsage =
  Usage'
    { usagePlanId = Prelude.Nothing,
      endDate = Prelude.Nothing,
      items = Prelude.Nothing,
      startDate = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | The plan Id associated with this usage data.
usage_usagePlanId :: Lens.Lens' Usage (Prelude.Maybe Prelude.Text)
usage_usagePlanId = Lens.lens (\Usage' {usagePlanId} -> usagePlanId) (\s@Usage' {} a -> s {usagePlanId = a} :: Usage)

-- | The ending date of the usage data.
usage_endDate :: Lens.Lens' Usage (Prelude.Maybe Prelude.Text)
usage_endDate = Lens.lens (\Usage' {endDate} -> endDate) (\s@Usage' {} a -> s {endDate = a} :: Usage)

-- | The usage data, as daily logs of used and remaining quotas, over the
-- specified time interval indexed over the API keys in a usage plan. For
-- example,
-- @{..., \"values\" : { \"{api_key}\" : [ [0, 100], [10, 90], [100, 10]]}@,
-- where @{api_key}@ stands for an API key value and the daily log entry is
-- of the format @[used quota, remaining quota]@.
usage_items :: Lens.Lens' Usage (Prelude.Maybe (Prelude.HashMap Prelude.Text [[Prelude.Integer]]))
usage_items = Lens.lens (\Usage' {items} -> items) (\s@Usage' {} a -> s {items = a} :: Usage) Prelude.. Lens.mapping Lens.coerced

-- | The starting date of the usage data.
usage_startDate :: Lens.Lens' Usage (Prelude.Maybe Prelude.Text)
usage_startDate = Lens.lens (\Usage' {startDate} -> startDate) (\s@Usage' {} a -> s {startDate = a} :: Usage)

-- | Undocumented member.
usage_position :: Lens.Lens' Usage (Prelude.Maybe Prelude.Text)
usage_position = Lens.lens (\Usage' {position} -> position) (\s@Usage' {} a -> s {position = a} :: Usage)

instance Core.FromJSON Usage where
  parseJSON =
    Core.withObject
      "Usage"
      ( \x ->
          Usage'
            Prelude.<$> (x Core..:? "usagePlanId")
            Prelude.<*> (x Core..:? "endDate")
            Prelude.<*> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "startDate")
            Prelude.<*> (x Core..:? "position")
      )

instance Prelude.Hashable Usage where
  hashWithSalt salt' Usage' {..} =
    salt' `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` usagePlanId

instance Prelude.NFData Usage where
  rnf Usage' {..} =
    Prelude.rnf usagePlanId
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf items
      `Prelude.seq` Prelude.rnf endDate
