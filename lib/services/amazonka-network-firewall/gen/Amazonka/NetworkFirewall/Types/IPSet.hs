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
-- Module      : Amazonka.NetworkFirewall.Types.IPSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.IPSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of IP addresses and address ranges, in CIDR notation. This is
-- part of a RuleVariables.
--
-- /See:/ 'newIPSet' smart constructor.
data IPSet = IPSet'
  { -- | The list of IP addresses and address ranges, in CIDR notation.
    definition :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'iPSet_definition' - The list of IP addresses and address ranges, in CIDR notation.
newIPSet ::
  IPSet
newIPSet = IPSet' {definition = Prelude.mempty}

-- | The list of IP addresses and address ranges, in CIDR notation.
iPSet_definition :: Lens.Lens' IPSet [Prelude.Text]
iPSet_definition = Lens.lens (\IPSet' {definition} -> definition) (\s@IPSet' {} a -> s {definition = a} :: IPSet) Prelude.. Lens.coerced

instance Core.FromJSON IPSet where
  parseJSON =
    Core.withObject
      "IPSet"
      ( \x ->
          IPSet'
            Prelude.<$> (x Core..:? "Definition" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable IPSet where
  hashWithSalt salt' IPSet' {..} =
    salt' `Prelude.hashWithSalt` definition

instance Prelude.NFData IPSet where
  rnf IPSet' {..} = Prelude.rnf definition

instance Core.ToJSON IPSet where
  toJSON IPSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Definition" Core..= definition)]
      )
