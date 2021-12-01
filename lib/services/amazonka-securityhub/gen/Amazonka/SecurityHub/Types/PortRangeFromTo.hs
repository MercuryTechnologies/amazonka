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
-- Module      : Amazonka.SecurityHub.Types.PortRangeFromTo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PortRangeFromTo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A range of ports.
--
-- /See:/ 'newPortRangeFromTo' smart constructor.
data PortRangeFromTo = PortRangeFromTo'
  { -- | The last port in the port range.
    to :: Prelude.Maybe Prelude.Int,
    -- | The first port in the port range.
    from :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortRangeFromTo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'to', 'portRangeFromTo_to' - The last port in the port range.
--
-- 'from', 'portRangeFromTo_from' - The first port in the port range.
newPortRangeFromTo ::
  PortRangeFromTo
newPortRangeFromTo =
  PortRangeFromTo'
    { to = Prelude.Nothing,
      from = Prelude.Nothing
    }

-- | The last port in the port range.
portRangeFromTo_to :: Lens.Lens' PortRangeFromTo (Prelude.Maybe Prelude.Int)
portRangeFromTo_to = Lens.lens (\PortRangeFromTo' {to} -> to) (\s@PortRangeFromTo' {} a -> s {to = a} :: PortRangeFromTo)

-- | The first port in the port range.
portRangeFromTo_from :: Lens.Lens' PortRangeFromTo (Prelude.Maybe Prelude.Int)
portRangeFromTo_from = Lens.lens (\PortRangeFromTo' {from} -> from) (\s@PortRangeFromTo' {} a -> s {from = a} :: PortRangeFromTo)

instance Core.FromJSON PortRangeFromTo where
  parseJSON =
    Core.withObject
      "PortRangeFromTo"
      ( \x ->
          PortRangeFromTo'
            Prelude.<$> (x Core..:? "To") Prelude.<*> (x Core..:? "From")
      )

instance Prelude.Hashable PortRangeFromTo where
  hashWithSalt salt' PortRangeFromTo' {..} =
    salt' `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData PortRangeFromTo where
  rnf PortRangeFromTo' {..} =
    Prelude.rnf to `Prelude.seq` Prelude.rnf from

instance Core.ToJSON PortRangeFromTo where
  toJSON PortRangeFromTo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("To" Core..=) Prelude.<$> to,
            ("From" Core..=) Prelude.<$> from
          ]
      )
