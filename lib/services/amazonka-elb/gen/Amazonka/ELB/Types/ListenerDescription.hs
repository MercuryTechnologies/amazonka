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
-- Module      : Amazonka.ELB.Types.ListenerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.ListenerDescription where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.Listener
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The policies enabled for a listener.
--
-- /See:/ 'newListenerDescription' smart constructor.
data ListenerDescription = ListenerDescription'
  { -- | The policies. If there are no policies enabled, the list is empty.
    policyNames :: Prelude.Maybe [Prelude.Text],
    -- | The listener.
    listener :: Prelude.Maybe Listener
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyNames', 'listenerDescription_policyNames' - The policies. If there are no policies enabled, the list is empty.
--
-- 'listener', 'listenerDescription_listener' - The listener.
newListenerDescription ::
  ListenerDescription
newListenerDescription =
  ListenerDescription'
    { policyNames = Prelude.Nothing,
      listener = Prelude.Nothing
    }

-- | The policies. If there are no policies enabled, the list is empty.
listenerDescription_policyNames :: Lens.Lens' ListenerDescription (Prelude.Maybe [Prelude.Text])
listenerDescription_policyNames = Lens.lens (\ListenerDescription' {policyNames} -> policyNames) (\s@ListenerDescription' {} a -> s {policyNames = a} :: ListenerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The listener.
listenerDescription_listener :: Lens.Lens' ListenerDescription (Prelude.Maybe Listener)
listenerDescription_listener = Lens.lens (\ListenerDescription' {listener} -> listener) (\s@ListenerDescription' {} a -> s {listener = a} :: ListenerDescription)

instance Core.FromXML ListenerDescription where
  parseXML x =
    ListenerDescription'
      Prelude.<$> ( x Core..@? "PolicyNames" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Listener")

instance Prelude.Hashable ListenerDescription where
  hashWithSalt salt' ListenerDescription' {..} =
    salt' `Prelude.hashWithSalt` listener
      `Prelude.hashWithSalt` policyNames

instance Prelude.NFData ListenerDescription where
  rnf ListenerDescription' {..} =
    Prelude.rnf policyNames
      `Prelude.seq` Prelude.rnf listener
