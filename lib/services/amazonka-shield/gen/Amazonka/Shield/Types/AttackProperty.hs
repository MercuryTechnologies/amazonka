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
-- Module      : Amazonka.Shield.Types.AttackProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AttackLayer
import Amazonka.Shield.Types.AttackPropertyIdentifier
import Amazonka.Shield.Types.Contributor
import Amazonka.Shield.Types.Unit

-- | Details of a Shield event. This is provided as part of an AttackDetail.
--
-- /See:/ 'newAttackProperty' smart constructor.
data AttackProperty = AttackProperty'
  { -- | The type of Shield event that was observed. @NETWORK@ indicates layer 3
    -- and layer 4 events and @APPLICATION@ indicates layer 7 events.
    --
    -- For infrastructure layer events (L3 and L4 events) after January 25,
    -- 2021, you can view metrics for top contributors in Amazon CloudWatch
    -- metrics. For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
    -- in the /WAF Developer Guide/.
    attackLayer :: Prelude.Maybe AttackLayer,
    -- | Contributor objects for the top five contributors to a Shield event.
    topContributors :: Prelude.Maybe [Contributor],
    -- | Defines the Shield event property information that is provided. The
    -- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
    -- are valid only for WordPress reflective pingback events.
    attackPropertyIdentifier :: Prelude.Maybe AttackPropertyIdentifier,
    -- | The total contributions made to this Shield event by all contributors.
    total :: Prelude.Maybe Prelude.Integer,
    -- | The unit used for the @Contributor@ @Value@ property.
    unit :: Prelude.Maybe Unit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attackLayer', 'attackProperty_attackLayer' - The type of Shield event that was observed. @NETWORK@ indicates layer 3
-- and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- For infrastructure layer events (L3 and L4 events) after January 25,
-- 2021, you can view metrics for top contributors in Amazon CloudWatch
-- metrics. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
-- in the /WAF Developer Guide/.
--
-- 'topContributors', 'attackProperty_topContributors' - Contributor objects for the top five contributors to a Shield event.
--
-- 'attackPropertyIdentifier', 'attackProperty_attackPropertyIdentifier' - Defines the Shield event property information that is provided. The
-- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
-- are valid only for WordPress reflective pingback events.
--
-- 'total', 'attackProperty_total' - The total contributions made to this Shield event by all contributors.
--
-- 'unit', 'attackProperty_unit' - The unit used for the @Contributor@ @Value@ property.
newAttackProperty ::
  AttackProperty
newAttackProperty =
  AttackProperty'
    { attackLayer = Prelude.Nothing,
      topContributors = Prelude.Nothing,
      attackPropertyIdentifier = Prelude.Nothing,
      total = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The type of Shield event that was observed. @NETWORK@ indicates layer 3
-- and layer 4 events and @APPLICATION@ indicates layer 7 events.
--
-- For infrastructure layer events (L3 and L4 events) after January 25,
-- 2021, you can view metrics for top contributors in Amazon CloudWatch
-- metrics. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/monitoring-cloudwatch.html#set-ddos-alarms Shield metrics and alarms>
-- in the /WAF Developer Guide/.
attackProperty_attackLayer :: Lens.Lens' AttackProperty (Prelude.Maybe AttackLayer)
attackProperty_attackLayer = Lens.lens (\AttackProperty' {attackLayer} -> attackLayer) (\s@AttackProperty' {} a -> s {attackLayer = a} :: AttackProperty)

-- | Contributor objects for the top five contributors to a Shield event.
attackProperty_topContributors :: Lens.Lens' AttackProperty (Prelude.Maybe [Contributor])
attackProperty_topContributors = Lens.lens (\AttackProperty' {topContributors} -> topContributors) (\s@AttackProperty' {} a -> s {topContributors = a} :: AttackProperty) Prelude.. Lens.mapping Lens.coerced

-- | Defines the Shield event property information that is provided. The
-- @WORDPRESS_PINGBACK_REFLECTOR@ and @WORDPRESS_PINGBACK_SOURCE@ values
-- are valid only for WordPress reflective pingback events.
attackProperty_attackPropertyIdentifier :: Lens.Lens' AttackProperty (Prelude.Maybe AttackPropertyIdentifier)
attackProperty_attackPropertyIdentifier = Lens.lens (\AttackProperty' {attackPropertyIdentifier} -> attackPropertyIdentifier) (\s@AttackProperty' {} a -> s {attackPropertyIdentifier = a} :: AttackProperty)

-- | The total contributions made to this Shield event by all contributors.
attackProperty_total :: Lens.Lens' AttackProperty (Prelude.Maybe Prelude.Integer)
attackProperty_total = Lens.lens (\AttackProperty' {total} -> total) (\s@AttackProperty' {} a -> s {total = a} :: AttackProperty)

-- | The unit used for the @Contributor@ @Value@ property.
attackProperty_unit :: Lens.Lens' AttackProperty (Prelude.Maybe Unit)
attackProperty_unit = Lens.lens (\AttackProperty' {unit} -> unit) (\s@AttackProperty' {} a -> s {unit = a} :: AttackProperty)

instance Core.FromJSON AttackProperty where
  parseJSON =
    Core.withObject
      "AttackProperty"
      ( \x ->
          AttackProperty'
            Prelude.<$> (x Core..:? "AttackLayer")
            Prelude.<*> ( x Core..:? "TopContributors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AttackPropertyIdentifier")
            Prelude.<*> (x Core..:? "Total")
            Prelude.<*> (x Core..:? "Unit")
      )

instance Prelude.Hashable AttackProperty where
  hashWithSalt salt' AttackProperty' {..} =
    salt' `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` attackPropertyIdentifier
      `Prelude.hashWithSalt` topContributors
      `Prelude.hashWithSalt` attackLayer

instance Prelude.NFData AttackProperty where
  rnf AttackProperty' {..} =
    Prelude.rnf attackLayer
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf attackPropertyIdentifier
      `Prelude.seq` Prelude.rnf topContributors
