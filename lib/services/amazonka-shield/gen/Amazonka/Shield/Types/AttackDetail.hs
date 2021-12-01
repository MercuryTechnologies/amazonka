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
-- Module      : Amazonka.Shield.Types.AttackDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AttackProperty
import Amazonka.Shield.Types.Mitigation
import Amazonka.Shield.Types.SubResourceSummary
import Amazonka.Shield.Types.SummarizedCounter

-- | The details of a DDoS attack.
--
-- /See:/ 'newAttackDetail' smart constructor.
data AttackDetail = AttackDetail'
  { -- | The unique identifier (ID) of the attack.
    attackId :: Prelude.Maybe Prelude.Text,
    -- | The time the attack started, in Unix time in seconds. For more
    -- information see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | If applicable, additional detail about the resource being attacked, for
    -- example, IP address or URL.
    subResources :: Prelude.Maybe [SubResourceSummary],
    -- | List of mitigation actions taken for the attack.
    mitigations :: Prelude.Maybe [Mitigation],
    -- | The array of objects that provide details of the Shield event.
    --
    -- For infrastructure layer events (L3 and L4 events) after January 25,
    -- 2021, you can view metrics for top contributors in Amazon CloudWatch
    -- metrics. For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
    -- in the /WAF Developer Guide/.
    attackProperties :: Prelude.Maybe [AttackProperty],
    -- | List of counters that describe the attack for the specified time period.
    attackCounters :: Prelude.Maybe [SummarizedCounter],
    -- | The ARN (Amazon Resource Name) of the resource that was attacked.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The time the attack ended, in Unix time in seconds. For more information
    -- see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
    endTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attackId', 'attackDetail_attackId' - The unique identifier (ID) of the attack.
--
-- 'startTime', 'attackDetail_startTime' - The time the attack started, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
--
-- 'subResources', 'attackDetail_subResources' - If applicable, additional detail about the resource being attacked, for
-- example, IP address or URL.
--
-- 'mitigations', 'attackDetail_mitigations' - List of mitigation actions taken for the attack.
--
-- 'attackProperties', 'attackDetail_attackProperties' - The array of objects that provide details of the Shield event.
--
-- For infrastructure layer events (L3 and L4 events) after January 25,
-- 2021, you can view metrics for top contributors in Amazon CloudWatch
-- metrics. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
-- in the /WAF Developer Guide/.
--
-- 'attackCounters', 'attackDetail_attackCounters' - List of counters that describe the attack for the specified time period.
--
-- 'resourceArn', 'attackDetail_resourceArn' - The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- 'endTime', 'attackDetail_endTime' - The time the attack ended, in Unix time in seconds. For more information
-- see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
newAttackDetail ::
  AttackDetail
newAttackDetail =
  AttackDetail'
    { attackId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      subResources = Prelude.Nothing,
      mitigations = Prelude.Nothing,
      attackProperties = Prelude.Nothing,
      attackCounters = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The unique identifier (ID) of the attack.
attackDetail_attackId :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.Text)
attackDetail_attackId = Lens.lens (\AttackDetail' {attackId} -> attackId) (\s@AttackDetail' {} a -> s {attackId = a} :: AttackDetail)

-- | The time the attack started, in Unix time in seconds. For more
-- information see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackDetail_startTime :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.UTCTime)
attackDetail_startTime = Lens.lens (\AttackDetail' {startTime} -> startTime) (\s@AttackDetail' {} a -> s {startTime = a} :: AttackDetail) Prelude.. Lens.mapping Core._Time

-- | If applicable, additional detail about the resource being attacked, for
-- example, IP address or URL.
attackDetail_subResources :: Lens.Lens' AttackDetail (Prelude.Maybe [SubResourceSummary])
attackDetail_subResources = Lens.lens (\AttackDetail' {subResources} -> subResources) (\s@AttackDetail' {} a -> s {subResources = a} :: AttackDetail) Prelude.. Lens.mapping Lens.coerced

-- | List of mitigation actions taken for the attack.
attackDetail_mitigations :: Lens.Lens' AttackDetail (Prelude.Maybe [Mitigation])
attackDetail_mitigations = Lens.lens (\AttackDetail' {mitigations} -> mitigations) (\s@AttackDetail' {} a -> s {mitigations = a} :: AttackDetail) Prelude.. Lens.mapping Lens.coerced

-- | The array of objects that provide details of the Shield event.
--
-- For infrastructure layer events (L3 and L4 events) after January 25,
-- 2021, you can view metrics for top contributors in Amazon CloudWatch
-- metrics. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
-- in the /WAF Developer Guide/.
attackDetail_attackProperties :: Lens.Lens' AttackDetail (Prelude.Maybe [AttackProperty])
attackDetail_attackProperties = Lens.lens (\AttackDetail' {attackProperties} -> attackProperties) (\s@AttackDetail' {} a -> s {attackProperties = a} :: AttackDetail) Prelude.. Lens.mapping Lens.coerced

-- | List of counters that describe the attack for the specified time period.
attackDetail_attackCounters :: Lens.Lens' AttackDetail (Prelude.Maybe [SummarizedCounter])
attackDetail_attackCounters = Lens.lens (\AttackDetail' {attackCounters} -> attackCounters) (\s@AttackDetail' {} a -> s {attackCounters = a} :: AttackDetail) Prelude.. Lens.mapping Lens.coerced

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
attackDetail_resourceArn :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.Text)
attackDetail_resourceArn = Lens.lens (\AttackDetail' {resourceArn} -> resourceArn) (\s@AttackDetail' {} a -> s {resourceArn = a} :: AttackDetail)

-- | The time the attack ended, in Unix time in seconds. For more information
-- see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp>.
attackDetail_endTime :: Lens.Lens' AttackDetail (Prelude.Maybe Prelude.UTCTime)
attackDetail_endTime = Lens.lens (\AttackDetail' {endTime} -> endTime) (\s@AttackDetail' {} a -> s {endTime = a} :: AttackDetail) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AttackDetail where
  parseJSON =
    Core.withObject
      "AttackDetail"
      ( \x ->
          AttackDetail'
            Prelude.<$> (x Core..:? "AttackId")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "SubResources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Mitigations" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AttackProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AttackCounters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "EndTime")
      )

instance Prelude.Hashable AttackDetail where
  hashWithSalt salt' AttackDetail' {..} =
    salt' `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` attackCounters
      `Prelude.hashWithSalt` attackProperties
      `Prelude.hashWithSalt` mitigations
      `Prelude.hashWithSalt` subResources
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` attackId

instance Prelude.NFData AttackDetail where
  rnf AttackDetail' {..} =
    Prelude.rnf attackId
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf attackCounters
      `Prelude.seq` Prelude.rnf attackProperties
      `Prelude.seq` Prelude.rnf mitigations
      `Prelude.seq` Prelude.rnf subResources
      `Prelude.seq` Prelude.rnf startTime
