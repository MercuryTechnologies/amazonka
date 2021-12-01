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
-- Module      : Amazonka.ElasticBeanstalk.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.LoadBalancerDescription where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types.Listener
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the details of a LoadBalancer.
--
-- /See:/ 'newLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { -- | The name of the LoadBalancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the LoadBalancer.
    domain :: Prelude.Maybe Prelude.Text,
    -- | A list of Listeners used by the LoadBalancer.
    listeners :: Prelude.Maybe [Listener]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'loadBalancerDescription_loadBalancerName' - The name of the LoadBalancer.
--
-- 'domain', 'loadBalancerDescription_domain' - The domain name of the LoadBalancer.
--
-- 'listeners', 'loadBalancerDescription_listeners' - A list of Listeners used by the LoadBalancer.
newLoadBalancerDescription ::
  LoadBalancerDescription
newLoadBalancerDescription =
  LoadBalancerDescription'
    { loadBalancerName =
        Prelude.Nothing,
      domain = Prelude.Nothing,
      listeners = Prelude.Nothing
    }

-- | The name of the LoadBalancer.
loadBalancerDescription_loadBalancerName :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_loadBalancerName = Lens.lens (\LoadBalancerDescription' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerDescription' {} a -> s {loadBalancerName = a} :: LoadBalancerDescription)

-- | The domain name of the LoadBalancer.
loadBalancerDescription_domain :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_domain = Lens.lens (\LoadBalancerDescription' {domain} -> domain) (\s@LoadBalancerDescription' {} a -> s {domain = a} :: LoadBalancerDescription)

-- | A list of Listeners used by the LoadBalancer.
loadBalancerDescription_listeners :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [Listener])
loadBalancerDescription_listeners = Lens.lens (\LoadBalancerDescription' {listeners} -> listeners) (\s@LoadBalancerDescription' {} a -> s {listeners = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      Prelude.<$> (x Core..@? "LoadBalancerName")
      Prelude.<*> (x Core..@? "Domain")
      Prelude.<*> ( x Core..@? "Listeners" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable LoadBalancerDescription where
  hashWithSalt salt' LoadBalancerDescription' {..} =
    salt' `Prelude.hashWithSalt` listeners
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` loadBalancerName

instance Prelude.NFData LoadBalancerDescription where
  rnf LoadBalancerDescription' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf listeners
      `Prelude.seq` Prelude.rnf domain
